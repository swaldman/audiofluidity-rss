package audiofluidity.rss.atom

import java.time.{ZonedDateTime,ZoneId}
import java.time.format.{DateTimeFormatter,DateTimeParseException}
import scala.xml.*
import audiofluidity.rss.{BadAtomXml,ConflictingNamespaces,Element,Namespace}
import audiofluidity.rss.util.stripPrefixedNamespaces

private def mbUniqueText(parent : Elem, label : String) : Option[String] =
  val nseq = parent \ label
  if nseq.size == 1 then
    Some(nseq.head.text)
  else
    None

private def uniqueText(parent : Elem, label : String) : String =
  mbUniqueText(parent, label).getOrElse:
    throw new BadAtomXml(s"Expected, did not find, a unique '${label}' in '${parent.label}'.")

private def findPubDate(entryElem : Elem) : ZonedDateTime =
  val isoOffsetOrInstant =
    (mbUniqueText( entryElem, "published" ) orElse mbUniqueText( entryElem, "updated")).getOrElse:
      throw new BadAtomXml(s"Found neither a 'published' nor 'updated' element in entry.")
  parseDateConstruct(isoOffsetOrInstant)    

def parseDateConstruct( isoOffsetOrInstant : String ) : ZonedDateTime =
  try
    ZonedDateTime.parse( isoOffsetOrInstant, DateTimeFormatter.ISO_OFFSET_DATE_TIME )
  catch  
    case dtpe : DateTimeParseException => ZonedDateTime.from( DateTimeFormatter.ISO_INSTANT.withZone(ZoneId.systemDefault()).parse(isoOffsetOrInstant) )

private val DefaultAuthorEmail = "nospam@dev.null"

private def namesToNameStr( names : Seq[String] ) : Option[String] =
  names.size match
    case 0 => None
    case 1 => Some( names.head )
    case 2 => Some( names.mkString(" and ") )
    case _ =>
      val modifiedNames = names.init :+ s"and ${names.last}"
      Some( modifiedNames.mkString(", ") )

private def rssAuthor( entryElem : Elem, defaultAuthor : Option[String] = None ) : String =
  //println( s"rssAuthor( entryElem, $defaultAuthor )" )
  val entryAuthors = entryElem \ "author"
  val email =
    if entryAuthors.length > 1 || entryAuthors.isEmpty then
      DefaultAuthorEmail
    else
      mbUniqueText( entryAuthors.head.asInstanceOf[Elem], "email" ).getOrElse( DefaultAuthorEmail )
  val names = entryAuthors.map( authorElem => mbUniqueText(authorElem.asInstanceOf[Elem], "name") ).collect { case Some(name) => name }
  val mbNamesStr = namesToNameStr( names ) orElse defaultAuthor
  val suffix = mbNamesStr.fold("")(str => s" (${str})")
  s"""${email}${suffix}"""

// We use atomizeUnclaimedRecursiveNoPrefixedNamespaces(...), which relies upon all labeled namespaces
// having been recursively stripped
private def itemElemFromEntryElem( entryElem : Elem, defaultAuthor : Option[String] = None ) : Elem =
  val title = uniqueText( entryElem, "title")
  val guid = uniqueText(entryElem, "id")
  val link = (entryElem \ "link").filter( node => node \@ "rel" == "alternate" || node \@ "rel" == "").head \@ "href"
  val description = mbUniqueText( entryElem, "summary" ).getOrElse(s"Entry entitled '${title}.'") // really lame, but do I want to bring in jsoup to summarize contents?
  val author = rssAuthor( entryElem, defaultAuthor )
  val pubDate = findPubDate( entryElem )
  val categories =
    (entryElem \ "category").map( _ \@ "term" ).filter( _ != null ).map( _.trim ).filter( _.nonEmpty ).map( c => Element.Category(null, c) )
  val mbContents = mbUniqueText( entryElem, "content" )
  val guidIsPermalink = guid == link
  val simpleItem =
    Element.Item.create(
      title = title,
      linkUrl = link,
      description = description,
      author = author,
      guid = Some(Element.Guid(guidIsPermalink, guid)),
      categories = categories,
      pubDate = Some(pubDate)
    )
  val passThruElems : Seq[Elem] = entryElem.child.collect{ case e : Elem => e }.filter( _.label != "content" ).map( atomizeUnclaimedRecursiveNoPrefixedNamespaces )
  val mbExtraContentEncodedElem =
    if passThruElems.contains( (elem : Elem) => elem.prefix == "content" && elem.label == "encoded" ) then None
    else
      mbContents match
        case Some(text) => Some(Element.Content.Encoded(text))
        case None       => None
  val withAnyExtraContent = mbExtraContentEncodedElem.fold(simpleItem)( element => simpleItem.withExtra( element ) )
  withAnyExtraContent.withExtras( passThruElems ).toElem

// this function depends on unprefixed namespaces being stripped first,
// from root and recursively!
private def atomizeUnclaimedRecursiveNoPrefixedNamespaces( elem : Elem ) : Elem =
  val newKids =
    elem.child.map { node =>
      node match
        case e     : Elem => atomizeUnclaimedRecursiveNoPrefixedNamespaces(e)
        case other : Node => other
    }
  if elem.prefix == null && elem.scope == TopScope then
    println( elem )
    elem.copy( prefix = "atom", child = newKids )
  else
    elem.copy( child = newKids )

private val AlwaysNamespaces = Set( Namespace.Atom, Namespace.RdfContent )

def rssElementFromAtomFeedElem( atomFeedElem : Elem ) : Element.Rss =
  val origNamespaces = Namespace.fromElemRecursive( atomFeedElem )
  val atomFeedElemNoLabels = stripPrefixedNamespaces( atomFeedElem ).asInstanceOf[Elem]
  val topTitle = uniqueText( atomFeedElemNoLabels, "title" )
  val topLink = (atomFeedElemNoLabels \ "link").filter( node => node \@ "rel" == "alternate" || node \@ "rel" == "" || node \@ "rel" == null ).head \@ "href"
  val defaultAuthor =
    val mbNames = (atomFeedElemNoLabels \ "author").map( authorElem => mbUniqueText(authorElem.asInstanceOf[Elem], "name" ) )
    val names = mbNames.collect { case Some(name) => name }
    namesToNameStr(names)
  val description = mbUniqueText(atomFeedElemNoLabels, "subtitle").getOrElse(s"RSS feed converstion of atom feed '${topTitle}'")
  val items : Seq[Elem] = (atomFeedElemNoLabels \ "entry").map ( node => itemElemFromEntryElem( node.asInstanceOf[Elem], defaultAuthor ) )
  val passThruElems = atomFeedElemNoLabels.child.collect{ case e : Elem => e}.filter( _.label != "entry").map( atomizeUnclaimedRecursiveNoPrefixedNamespaces )
  val channel = Element.Channel(
      title = Element.Title(topTitle),
      link = Element.Link(topLink),
      generator = Some(Element.Generator("audiofluidity-rss, atom-to-rss converter")),
      description = Element.Description(description),
      items=Nil
    ).withExtras(items).withExtras( passThruElems )
  val finalNamespaces =
    val excludingConflicts = Namespace.canonicalizeAll(origNamespaces.filter( _.prefix != None ) ++ AlwaysNamespaces)
    if excludingConflicts.excludedNamespaces.nonEmpty then
      throw new ConflictingNamespaces("The following namespaces from converted atom feed conflict: " + excludingConflicts.excludedNamespaces.mkString("; "))
    else
      excludingConflicts.withUniquePrefixes
  Element.Rss( channel ).overNamespaces( finalNamespaces.toList )

def rssElemFromAtomFeedElem( atomFeedElem : Elem ) : Elem = rssElementFromAtomFeedElem( atomFeedElem ).toElem

