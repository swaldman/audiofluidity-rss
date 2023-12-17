package audiofluidity.rss

import com.mchange.conveniences.string.*
import scala.annotation.tailrec
import scala.xml.{Elem,NamespaceBinding,TopScope}

object Namespace:
  final case class ExcludingConflicts( withUniquePrefixes : Set[Namespace], excluded : Map[Option[String],Set[Namespace]] ):
    lazy val excludedNamespaces = excluded.foldLeft( Set.empty[Namespace] )( (accum, nextTup) => accum ++ nextTup(1) )

  val RdfContent      = Namespace("content", "http://purl.org/rss/1.0/modules/content/")
  val ApplePodcast    = Namespace("itunes",  "http://www.itunes.com/dtds/podcast-1.0.dtd")
  val DublinCore      = Namespace("dc",      "http://purl.org/dc/elements/1.1/")
  val Atom            = Namespace("atom",    "http://www.w3.org/2005/Atom")
  val Podcast         = Namespace("podcast", "https://podcastindex.org/namespace/1.0")
  val Spotify         = Namespace("spotify", "http://www.spotify.com/ns/rss")
  val Media           = Namespace("media",   "http://search.yahoo.com/mrss/") // see https://www.rssboard.org/media-rss
  val CreativeCommons = Namespace("cc",      "http://web.resource.org/cc/")
  val Source          = Namespace("source",  "http://source.scripting.com/") // see http://source.scripting.com/

  // CreativeCommons namespace declaration variations
  //   http://blogs.law.harvard.edu/tech/creativeCommonsRssModule
  //     (see https://cyber.harvard.edu/rss/creativeCommonsRssModule.html)
  //   http://backend.userland.com/creativeCommonsRssModule
  //     (see https://cyber.harvard.edu/rss/examples/rssCreativeCommonsExample.xml)
  //   http://cyber.law.harvard.edu/rss/creativeCommonsRssModule.html
  //     (in the wild)
  //
  // Media namespace declaration variations
  //   https://www.rssboard.org/media-rss
  //     (in the wild)
  //   http://search.yahoo.com/rss
  //     (in the wild)
  //
  // Source namespace declaration variations
  //   http://source.smallpict.com/2014/07/12/theSourceNamespace.html
  //     (in the wild, an early version)

  def attemptCanonicalizeUri( uri : String ) : String =
    val normalizedUri =
      uri.dropWhile( _ != ':' ) // ignore http / https variation
        .reverse
        .dropWhile( _ == '/' ) // ignore trailing slashes
        .reverse

    normalizedUri match
      case "://www.w3.org/2005/Atom" => "http://www.w3.org/2005/Atom"
      case "://github.com/Podcastindex-org/podcast-namespace/blob/main/docs/1.0.md" => "https://podcastindex.org/namespace/1.0"
      case "://podcastindex.org/namespace/1.0" => "https://podcastindex.org/namespace/1.0"
      case "://podlove.org/simple-chapters" => "http://podlove.org/simple-chapters"
      case "://www.google.com/schemas/play-podcasts/1.0" => "http://www.google.com/schemas/play-podcasts/1.0"
      case "://blogs.law.harvard.edu/tech/creativeCommonsRssModule" => "http://web.resource.org/cc/"
      case "://backend.userland.com/creativeCommonsRssModule" => "http://web.resource.org/cc/"
      case "://cyber.law.harvard.edu/rss/creativeCommonsRssModule.html" => "http://web.resource.org/cc/"
      case "://www.rssboard.org/media-rss" => "http://search.yahoo.com/mrss/"
      case "://search.yahoo.com/rss" => "http://search.yahoo.com/mrss/"
      case "://source.smallpict.com/2014/07/12/theSourceNamespace.html" => "http://source.scripting.com/"
      case "://source.scripting.com" => "http://source.scripting.com/"
      case _ => uri

  private val ByPrefix =
    val commonNamespaces = RdfContent :: ApplePodcast :: DublinCore :: Atom :: Podcast :: Spotify :: Media :: CreativeCommons :: Source :: Nil
    commonNamespaces.map( ns => (ns.prefix, ns)).toMap

  def byPrefix( pfx : Option[String] ) : Option[Namespace] = ByPrefix.get( pfx )
  def byPrefix( pfx : String )         : Option[Namespace] = byPrefix( Some(pfx) )

  @tailrec
  private def namespaces( accum : List[Namespace], binding : NamespaceBinding ) : List[Namespace] =
    if binding == TopScope || binding == null then accum else namespaces( Namespace(binding.prefix, binding.uri ) :: accum, binding.parent)

  private def namespaces( accum : List[Namespace], bindingSeq : Seq[NamespaceBinding] ) : List[Namespace] =
    bindingSeq.foldLeft( accum )( (soFar, next) => namespaces( soFar, next ) )

  private def elemNamespacesRecursive( accum : List[Namespace], elems : Seq[Elem] ) : List[Namespace] =
    elems.foldLeft( accum )( (soFar, next) => namespaces( soFar, next.descendant_or_self.map( _.scope ) ) )

  private def elemNamespacesDirect( accum : List[Namespace], elems : Seq[Elem] ) : List[Namespace] =
    elems.foldLeft( accum )( (soFar, next) => namespaces( soFar, next.map( _.scope ) ) )

  def fromElemsRecursive( elems : Elem* ) : Set[Namespace] = elemNamespacesRecursive( Nil, elems ).toSet

  def fromElemRecursive( elem : Elem ) : Set[Namespace] = fromElemsRecursive(Seq(elem)*).toSet

  def fromElemsDirect( elems : Elem* ) : Set[Namespace] = elemNamespacesDirect( Nil, elems ).toSet

  def fromElemDirect( elem : Elem ) : Set[Namespace] = fromElemsDirect(Seq(elem)*).toSet

  def excludeConflicts( namespaces : Set[Namespace] ) : ExcludingConflicts =
    val prefixGrouped = namespaces.groupBy( _.prefix )
    prefixGrouped.foldLeft( ExcludingConflicts( Set.empty, Map.empty ) ): (accum, nextTup) =>
      nextTup(1).size match
        case 1 => accum.copy( withUniquePrefixes = accum.withUniquePrefixes ++ nextTup(1) )
        case _ => accum.copy( excluded = (accum.excluded + nextTup).toMap )

  def canonicalizeConflicts( excludingConflicts : ExcludingConflicts ) : ExcludingConflicts =
    val canonicalizedConflicts = excludingConflicts.excluded.map( ( prefix, set ) => ( prefix, set.map( _.canonical ) ) )
    val (resolved, remaining) = canonicalizedConflicts.partition( tup => tup(1).size == 1 )
    val resolvedNamespaces = resolved.foldLeft( Set.empty[Namespace] )( (accum, nextTup ) => accum ++ nextTup(1) )
    excludingConflicts.copy( withUniquePrefixes = excludingConflicts.withUniquePrefixes ++ resolvedNamespaces, excluded = remaining )

  def canonicalizeConflicts( namespaces : Set[Namespace] ) : ExcludingConflicts =
    val checkConflicts = excludeConflicts( namespaces )
    if checkConflicts.excluded.nonEmpty then canonicalizeConflicts(checkConflicts) else checkConflicts

  def canonicalizeAll( namespaces : Set[Namespace] ) : ExcludingConflicts =
    excludeConflicts( namespaces.map( _.canonical ) )

  private def toBinding( parentScope : NamespaceBinding, list : List[Namespace] ) : NamespaceBinding =
    list match
      case head :: tail => toBinding(new NamespaceBinding(head.prefix.getOrElse(null), head.uri, parentScope), tail)
      case Nil          => parentScope
  def toBinding( namespaces : List[Namespace]) : NamespaceBinding = toBinding(TopScope, namespaces)
  def toBinding( namespaces : Set[Namespace])  : NamespaceBinding = toBinding(TopScope, namespaces.toList)

  def apply( prefix : String, uri : String ) : Namespace = this( prefix.asOptionNotBlank, uri )

case class Namespace(prefix : Option[String], uri : String):
  def canonical : Namespace = Namespace(prefix, Namespace.attemptCanonicalizeUri(uri))

