package audiofluidity.rss.util

import scala.annotation.{nowarn,tailrec}
import scala.util.Try

import java.time.{Instant,ZonedDateTime}
import java.time.format.*
import java.time.temporal.{ChronoField,ChronoUnit}
import java.time.temporal.ChronoField.*
import java.time.chrono.IsoChronology
import scala.jdk.CollectionConverters._
import java.time.temporal.TemporalAccessor

import scala.xml.*
import audiofluidity.rss.Namespace

private val RssDateTimeFormatter = DateTimeFormatter.RFC_1123_DATE_TIME

// see https://stackoverflow.com/questions/45829799/java-time-format-datetimeformatter-rfc-1123-date-time-fails-to-parse-time-zone-n
private val LenientRssDateTimeFormatter =
  val dow = Map(1L->"Mon",2L->"Tue",3L->"Wed",4L->"Thu",5L->"Fri",6L->"Sat",7L->"Sun").map( (k,v) => (k.asInstanceOf[java.lang.Long],v) )
  val moy = Map(1L->"Jan",2L->"Feb",3L->"Mar",4L->"Apr",5L->"May",6L->"Jun",7L->"Jul",8L->"Aug",9L->"Sep",10L->"Oct",11L->"Nov",12L->"Dec").map( (k,v) => (k.asInstanceOf[java.lang.Long],v) )
  new DateTimeFormatterBuilder()
    .parseCaseInsensitive()
    .parseLenient()
    .optionalStart()
    .appendText(DAY_OF_WEEK, dow.asJava)
    .appendLiteral(", ")
    .optionalEnd()
    .appendValue(DAY_OF_MONTH, 1, 2, SignStyle.NOT_NEGATIVE)
    .appendLiteral(' ')
    .appendText(MONTH_OF_YEAR, moy.asJava)
    .appendLiteral(' ')
    .appendValue(YEAR, 4)  // 2 digit year not handled
    .appendLiteral(' ')
    .appendValue(HOUR_OF_DAY, 2)
    .appendLiteral(':')
    .appendValue(MINUTE_OF_HOUR, 2)
    .optionalStart()
    .appendLiteral(':')
    .appendValue(SECOND_OF_MINUTE, 2)
    .optionalEnd()
    .appendLiteral(' ')
    // difference from RFC_1123_DATE_TIME: optional offset OR zone ID
    .optionalStart()
    .appendZoneText(TextStyle.SHORT)
    .optionalEnd()
    .optionalStart()
    .appendOffset("+HHMM", "GMT")
    // use the same resolver style and chronology
    .toFormatter().withResolverStyle(ResolverStyle.SMART).withChronology(IsoChronology.INSTANCE)

private def _attemptLenientParsePubDate( str : String ) : Try[TemporalAccessor] =
  (Try(RssDateTimeFormatter.parse(str)) orElse Try(LenientRssDateTimeFormatter.parse(str)))

def attemptLenientParsePubDateToInstant( str : String ) : Try[Instant] =
  _attemptLenientParsePubDate( str ).map( Instant.from )

def attemptLenientParsePubDate( str : String ) : Try[ZonedDateTime] =
  _attemptLenientParsePubDate( str ).map( ZonedDateTime.from )

def formatPubDate( zdt : ZonedDateTime ) : String = RssDateTimeFormatter.format( zdt )

def formatAtomUpdated( instant : Instant ) : String =
  val millis = instant.get( ChronoField.MILLI_OF_SECOND )
  val trunc = instant.truncatedTo( ChronoUnit.SECONDS )
  val time = if millis >= 500 then trunc.plusSeconds(1) else trunc
  DateTimeFormatter.ISO_INSTANT.format( time )

/**
  * Convert an RSS feed into a similar feed except containing just a single item.
  *
  * @param rssElem the rss to recreate for a single item
  * @param retainGuid the guid of the single item
  * @param nonItemChannelChildrenFilter which channel-level elements to include, by default all, @see SkipUnstableChannelElements
  * @return either the single element RSS ot a String explaining why it could not be produced
  */
def singleItemRss( rssElem : Elem, retainGuid : String, nonItemChannelChildrenFilter : Elem => Boolean = _ => true, nonChannelRssChildrenFilter : Elem => Boolean = _ => true ) : Either[String,Elem] =
  if rssElem.label != "rss" then
    Left("Base element not RSS element: " + rssElem)
  else
    val channelElems = rssElem.child.collect { case elem : Elem if elem.label == "channel" => elem }
    if channelElems.size != 1 then
      Left("Should contain precisely one channele element, contains " + channelElems.size)
    else
      val channelElem = channelElems.head
      def hasGuid( itemElem : Elem, guid : String ) : Boolean =
        val guidElems = itemElem \ "guid"
        guidElems.exists( _.text.trim() == guid )
      val filteredChildren = channelElem.child.filter: node =>
        node match
          case elem : Elem if elem.label == "item" => hasGuid(elem, retainGuid)
          case elem : Elem => nonItemChannelChildrenFilter(elem)
          case _ => true
      (filteredChildren \ "item").size match
        case 0 =>
          Left(s"Item with guid '$retainGuid' not found!")
        case 1 =>
          val filteredChannelElem =
            channelElem.copy( child = filteredChildren )
          val newRssChildren = rssElem.child.flatMap: node =>
            node match
              case elem : Elem if elem.label == "channel" => Seq( filteredChannelElem )
              case elem : Elem if nonChannelRssChildrenFilter(elem) => Seq( elem )
              case elem : Elem => Seq.empty
              case other => other
          Right( rssElem.copy( child = newRssChildren ) )
        case n =>
          Left(s"Multiple ($n) items with guid '$retainGuid' found!")

/**
  * A filter predicate intended for use in the [[singleItemRss]] function.
  */
val SkipUnstableChannelElements = new Function1[Elem,Boolean]:
  def apply( elem : Elem ) = elem.label != "lastBuildDate" && elem.label != "pubDate"

def stripWhitespaceDirectChildren( parent : Elem ) : Elem =
  val trimmed = parent.child.map: n =>
    n match
      case t : Text => Text(t.text.trim)
      case other    => other
  val cleaned = trimmed.filter: n =>
    n match
      case t : Text => t.text.nonEmpty
      case other    => true
  parent.copy( child = cleaned )

def stripInsignificantWhitespaceRecursive( top : Elem, whitespaceSignificant : Elem => Boolean ) : Elem =
  def trim( elem : Elem ) : Elem =
    if whitespaceSignificant(elem) then
      elem
    else
      val newKids = elem.child.map: n =>
        n match
          case t : Text => Text(t.text.trim)
          case e : Elem => trim(e)
          case other    => other
      elem.copy( child = newKids )
  def filter( elem : Elem ) : Elem =
    if whitespaceSignificant(elem) then
      elem
    else
      val newKids = elem.child.filter: n =>
        n match
          case t : Text => t.text.nonEmpty
          case other    => true
      elem.copy( child = newKids )
  filter( trim( top ) )

def stripScopes(root: Node): Node =
  // from https://stackoverflow.com/questions/12535014/scala-completely-remove-namespace-from-xml
  def clearScope(x: Node): Node = x match {
    case e: Elem => e.copy(scope = TopScope, child = e.child.map(clearScope))
    case o => o
  }
  clearScope(root)

@tailrec
def includesUnprefixedNamespace( binding : NamespaceBinding ) : Boolean =
  if binding == TopScope then false
  else if binding.prefix == null then true
  else includesUnprefixedNamespace( binding.parent )

@tailrec
def unprefixedNamespaceOnly( binding : NamespaceBinding ) : NamespaceBinding =
  binding match
    case NamespaceBinding(null,   null, null  ) => TopScope
    case NamespaceBinding(null,   null, parent) => unprefixedNamespaceOnly( parent )
    case NamespaceBinding(null,    uri, _     ) => NamespaceBinding(null, uri, TopScope)
    case NamespaceBinding(prefix,    _, null  ) => TopScope
    case NamespaceBinding(prefix,    _, parent) => unprefixedNamespaceOnly( parent )


def stripPrefixedNamespaces( root : Node ) : Node =
  def clearPrefixedNamespaces(inheritedNsUri : String, x: Node): Node = x match {
    case e: Elem =>
      //println(s"See $e (scope ${e.scope})")
      val unprefixedOrTop = unprefixedNamespaceOnly(e.scope)
      if unprefixedOrTop.uri == null then // TopScope
        e.copy(scope = TopScope, child = e.child.map(n => clearPrefixedNamespaces(null, n)))
      else if unprefixedOrTop.uri == inheritedNsUri then  
        e.copy(scope = TopScope, child = e.child.map(n => clearPrefixedNamespaces(inheritedNsUri, n)))
      else // fresh unlabled namespace URI!
        e.copy(scope = unprefixedOrTop, child = e.child.map(n => clearPrefixedNamespaces(unprefixedOrTop.uri, n)))
    case o => o
  }
  clearPrefixedNamespaces(null, root)

@deprecated("Please use scopeContainsRaw(...)","0.0.7")
@tailrec
def scopeContains( prefix : String, uri : String, binding : NamespaceBinding ) : Boolean =
  if binding == TopScope then
    false
  else if prefix == binding.prefix && uri == binding.uri then
    true
  else
    scopeContains( prefix, uri, binding.parent )

@nowarn("cat=deprecation")
def scopeContainsRaw( prefix : String, uri : String, binding : NamespaceBinding ) : Boolean =
  scopeContains(prefix,uri,binding)

@nowarn("cat=deprecation")
def scopeContainsNamespace( namespace : Namespace, binding : NamespaceBinding ) : Boolean =
  namespace.prefix match
    case Some( pfx ) => scopeContains( pfx, namespace.uri, binding )
    case None        => scopeContains( null, namespace.uri, binding )

def scopeContainsNamespace( namespace : Namespace, namespaces : IterableOnce[Namespace] ) : Boolean =
  namespaces.iterator.contains(namespace)

private def uriCore( uri : String ) : String =
  uri.dropWhile( _ != ':' ).reverse.dropWhile( _ == '/' ).reverse

def scopeContainsRawLenient( prefix : String, uri : String, binding : NamespaceBinding ) : Boolean =
  def bindingUriCore = uriCore( binding.uri )
  if binding == TopScope then
    false
  else if prefix == binding.prefix && uri.contains(bindingUriCore) then
    true
  else
    scopeContainsRawLenient( prefix, uri, binding.parent )

def scopeContainsNamespaceLenient( namespace : Namespace, binding : NamespaceBinding ) : Boolean =
  namespace.prefix match
    case Some( pfx ) => scopeContainsRawLenient( pfx, namespace.uri, binding )
    case None        => scopeContainsRawLenient( null, namespace.uri, binding )

def scopeContainsNamespaceLenient( namespace : Namespace, namespaces : IterableOnce[Namespace] ) : Boolean =
  namespaces.iterator.find( checkMe => namespace.uri.contains( uriCore( checkMe.uri ) ) ).nonEmpty

def zeroOrOneChildElem( parent : Elem, label : String ) : Either[Seq[Elem],Option[Elem]] =
  val kids = (parent \ label)
  kids.size match
    case 0 => Right( None )
    case 1 => Right( Some(kids.head.asInstanceOf[Elem]) )
    case _ => Left( kids.map( _.asInstanceOf[Elem] ) )

extension (elem : Elem)
  def colorablyInNamespace( ns : Namespace ) = ns.prefix.fold(false)(_ == elem.prefix) || scopeContainsNamespaceLenient( ns.unprefixed, elem.scope )

