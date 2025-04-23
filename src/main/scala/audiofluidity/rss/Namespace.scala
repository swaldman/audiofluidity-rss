package audiofluidity.rss

import com.mchange.conveniences.string.*
import scala.annotation.tailrec
import scala.xml.{Elem,NamespaceBinding,TopScope}
import audiofluidity.rss.util.scopeContainsNamespaceLenient

object Namespace:
  final case class ExcludingConflicts( withUniquePrefixes : Set[Namespace], excluded : Map[Option[String],Set[Namespace]] ):
    lazy val excludedNamespaces = excluded.foldLeft( Set.empty[Namespace] )( (accum, nextTup) => accum ++ nextTup(1) )

  // when adding new well-known namespaces,
  // don't forget also to add to attemptCanonicalizeUri(...) and Common!

  val RdfContent      = Namespace("content", "http://purl.org/rss/1.0/modules/content/")
  val ApplePodcast    = Namespace("itunes",  "http://www.itunes.com/dtds/podcast-1.0.dtd")
  val DublinCore      = Namespace("dc",      "http://purl.org/dc/elements/1.1/")
  val Atom            = Namespace("atom",    "http://www.w3.org/2005/Atom")
  val Podcast         = Namespace("podcast", "https://podcastindex.org/namespace/1.0")
  val Spotify         = Namespace("spotify", "http://www.spotify.com/ns/rss")
  val Media           = Namespace("media",   "http://search.yahoo.com/mrss/") // see https://www.rssboard.org/media-rss
  val CreativeCommons = Namespace("cc",      "http://web.resource.org/cc/")
  val Source          = Namespace("source",  "http://source.scripting.com/") // see http://source.scripting.com/
  val WellFormedWeb   = Namespace("wfw",     "http://wellformedweb.org/CommentAPI/") // see https://www.rssboard.org/comment-api
  val Iffy            = Namespace("iffy",    "http://tech.interfluidity.com/xml/iffy/")
  val RawVoice        = Namespace("rawvoice","http://www.rawvoice.com/rawvoiceRssModule/") 

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
  //
  // See also https://podnews.net/article/additional-rss-namespaces-podcasting

  def attemptCanonicalizeUri( uri : String ) : String =
    ByUriLowerCase.get(uri.toLowerCase) match
      case Some( ns ) => ns.uri
      case None =>
        val normalizedUri =
          uri.dropWhile( _ != ':' ) // ignore http / https variation
            .reverse
            .dropWhile( _ == '/' ) // ignore trailing slashes
            .reverse

        normalizedUri match
          case "://www.w3.org/2005/Atom"                                                => Atom.uri
          case "://github.com/Podcastindex-org/podcast-namespace/blob/main/docs/1.0.md" => Podcast.uri
          case "://podcastindex.org/namespace/1.0"                                      => Podcast.uri
          case "://podlove.org/simple-chapters"                                         => "http://podlove.org/simple-chapters"
          case "://www.google.com/schemas/play-podcasts/1.0"                            => "http://www.google.com/schemas/play-podcasts/1.0"
          case "://blogs.law.harvard.edu/tech/creativeCommonsRssModule"                 => CreativeCommons.uri
          case "://backend.userland.com/creativeCommonsRssModule"                       => CreativeCommons.uri
          case "://cyber.law.harvard.edu/rss/creativeCommonsRssModule.html"             => CreativeCommons.uri
          case "://www.rssboard.org/media-rss"                                          => Media.uri
          case "://search.yahoo.com/rss"                                                => Media.uri
          case "://source.smallpict.com/2014/07/12/theSourceNamespace.html"             => Source.uri
          case "://source.scripting.com"                                                => Source.uri
          case "://wellformedweb.org/CommentAPI"                                        => WellFormedWeb.uri
          case "://tech.interfluidity.com/xml/iffy"                                     => Iffy.uri
          case "://www.rawvoice.com/rawvoiceRssModule"                                  => RawVoice.uri
          case "://blubrry.com/developer/rawvoice-rss"                                  => RawVoice.uri
          case _ => uri

  private val Common = RdfContent :: ApplePodcast :: DublinCore :: Atom :: Podcast :: Spotify :: Media :: CreativeCommons :: Source :: WellFormedWeb :: Iffy :: RawVoice :: Nil

  private val ByPrefix = Common.map( ns => (ns.prefix, ns) ).toMap

  private val ByUriLowerCase = Common.map( ns => ( ns.uri, ns) ).toMap

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

  def apply( prefix : String, uri : String ) : Namespace = this( prefix.toOptionNotBlank, uri )

  @tailrec
  private def _unrollBinding( accum : List[Namespace], binding : NamespaceBinding ) : List[Namespace] =
    if binding == TopScope then accum.reverse
    else _unrollBinding( Namespace(Option(binding.prefix),binding.uri) :: accum, binding.parent )

  def unrollBinding( binding : NamespaceBinding ) : List[Namespace] =
    _unrollBinding(Nil,binding)

  /**
    * @throws IncompleteNamespace iff elem has a prefix that can't be resolved
    */
  def guessForElem( elem : Elem ) : Option[Namespace] =
    val check = fromElemDirect(elem).find( ns => ns.belongsLenient(elem) ) orElse Common.find( ns => ns.belongsLenient(elem) )
    if check.isEmpty && elem.prefix != null then
      throw new IncompleteNamespace(s"Namespace URL for prefix '${elem.prefix}' could not be found or guessed.")
    else
      check

case class Namespace(prefix : Option[String], uri : String):
  def canonical  : Namespace = Namespace(prefix, Namespace.attemptCanonicalizeUri(uri))
  def unprefixed : Namespace = prefix.fold(this)(_ => Namespace(None, uri))
  def belongsLenient( elem : Elem ) : Boolean = elem.prefix == this.prefix.getOrElse(null) || scopeContainsNamespaceLenient(this.unprefixed, elem.scope)



