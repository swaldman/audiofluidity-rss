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
import audiofluidity.rss.{atom,Element,Namespace}

private val RssDateTimeFormatter = DateTimeFormatter.RFC_1123_DATE_TIME

private def _attemptLenientParsePubDate( str : String ) : Try[TemporalAccessor] =
  val trimmed = str.trim
  Try(RssDateTimeFormatter.parse(trimmed)) orElse LenientRfc1123.parse(trimmed)

def attemptLenientParsePubDateToInstant( str : String ) : Try[Instant] =
  _attemptLenientParsePubDate( str ).map( Instant.from )

def attemptLenientParsePubDate( str : String ) : Try[ZonedDateTime] =
  _attemptLenientParsePubDate( str ).map( ZonedDateTime.from )

def parseRssOrAtomDate( str : String ) : Try[ZonedDateTime] =
  attemptLenientParsePubDate(str).orElse(Try(atom.parseDateConstruct(str)))

def parseRssOrAtomDateToInstant( str : String ) : Try[Instant] =
  attemptLenientParsePubDate(str).orElse(Try(atom.parseDateConstruct(str))).map( Instant.from )

def formatPubDate( zdt : ZonedDateTime ) : String = RssDateTimeFormatter.format( zdt )

def roundToSecond( instant : Instant ) : Instant =
  val millis = instant.get( ChronoField.MILLI_OF_SECOND )
  val trunc = instant.truncatedTo( ChronoUnit.SECONDS )
  if millis >= 500 then trunc.plusSeconds(1) else trunc

def roundToSecond( zdt : ZonedDateTime ) : ZonedDateTime =
  val millis = zdt.get( ChronoField.MILLI_OF_SECOND )
  val trunc = zdt.truncatedTo( ChronoUnit.SECONDS )
  if millis >= 500 then trunc.plusSeconds(1) else trunc

def formatRFC3339ToSecond( instant : Instant ) : String =
  DateTimeFormatter.ISO_INSTANT.format( roundToSecond( instant ) )

def formatRFC3339ToSecond( zdt : ZonedDateTime ) : String =
  DateTimeFormatter.ISO_INSTANT.format( roundToSecond( zdt ) )

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
      val items = filteredChildren.collect { case elem : Elem if elem.label == "item" => elem }
      items.size match
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

@tailrec
def defaultNamespaceUri( binding : NamespaceBinding ) : Option[String] =
  if binding == TopScope then None
  else if binding.prefix == null then Some(binding.uri)
  else defaultNamespaceUri(binding.parent)

def zeroOrOneChildElem( parent : Elem, label : String ) : Either[Seq[Elem],Option[Elem]] =
  val kids = (parent \ label)
  kids.size match
    case 0 => Right( None )
    case 1 => Right( Some(kids.head.asInstanceOf[Elem]) )
    case _ => Left( kids.map( _.asInstanceOf[Elem] ) )

extension (elem : Elem)
  def colorablyInNamespace( ns : Namespace ) = ns.prefix.fold(false)(_ == elem.prefix) || scopeContainsNamespaceLenient( ns.unprefixed, elem.scope )

def normalizeNamespacePrefixes(elem: Elem, uriToPrefix : String => Option[String] ): Elem =
  def normalizeScope(scope: NamespaceBinding): NamespaceBinding =
    // Collect all bindings from the scope, normalizing prefixes as we go
    def collectBindings(s: NamespaceBinding): List[(String, String)] =
      if s == null || s == TopScope then Nil
      else
        val normalizedPrefix =
          if s.uri != null then uriToPrefix(s.uri).getOrElse(s.prefix)
          else s.prefix
        (normalizedPrefix, s.uri) :: collectBindings(s.parent)

    val bindings = collectBindings(scope)

    // Remove duplicates (keep first occurrence, which is the innermost binding)
    // Also deduplicate by both prefix and URI to avoid conflicts
    val seenPrefixes = scala.collection.mutable.Set[String]()
    val seenUris = scala.collection.mutable.Set[String]()

    val dedupedBindings = bindings.filter: (prefix, uri) =>
      val dominated = seenPrefixes.contains(prefix) || (uri != null && seenUris.contains(uri))
      if !dominated then
        seenPrefixes.add(prefix)
        if uri != null then seenUris.add(uri)
      !dominated

    // Rebuild the NamespaceBinding chain
    dedupedBindings.foldRight[NamespaceBinding](TopScope):
      case ((prefix, uri), parent) => NamespaceBinding(prefix, uri, parent)

  def normalizePrefix(currentPrefix: String, scope: NamespaceBinding): String =
    if currentPrefix == null then null
    else
      // Find the URI for this prefix in the original scope
      val uri = scope.getURI(currentPrefix)
      if uri != null then
        uriToPrefix(uri).getOrElse(currentPrefix)
      else currentPrefix

  def normalizeAttributes(attrs: MetaData, scope: NamespaceBinding): MetaData =
    attrs match
      case Null => Null
      case pa: PrefixedAttribute =>
        val uri = scope.getURI(pa.pre)
        val newPrefix = 
          if uri != null then 
            uriToPrefix(uri).getOrElse(pa.pre)
          else
            pa.pre
        PrefixedAttribute(
          newPrefix,
          pa.key,
          pa.value,
          normalizeAttributes(pa.next, scope)
        )
      case ua: UnprefixedAttribute =>
        UnprefixedAttribute(ua.key, ua.value, normalizeAttributes(ua.next, scope))

  def normalizeNode(node: Node, inheritedScope: NamespaceBinding): Node =
    node match
      case e: Elem =>
        // Merge element's scope with inherited scope for resolution
        val effectiveScope = if e.scope == null || e.scope == TopScope then inheritedScope else e.scope

        // Normalize the prefix for this element
        val newPrefix = normalizePrefix(e.prefix, effectiveScope)

        // Normalize the scope (xmlns declarations)
        val newScope = normalizeScope(effectiveScope)

        // Normalize attributes
        val newAttrs = normalizeAttributes(e.attributes, effectiveScope)

        // Recursively normalize children
        val newChildren = e.child.map(c => normalizeNode(c, newScope))

        e.copy(
          prefix = newPrefix,
          attributes = newAttrs,
          scope = newScope,
          child = newChildren
        )
      case other => other

  normalizeNode(elem, TopScope) match
    case e: Elem => e
    case _ => elem // Should never happen
