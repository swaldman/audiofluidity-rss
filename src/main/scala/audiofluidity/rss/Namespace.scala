package audiofluidity.rss

import scala.xml.{NamespaceBinding,TopScope}

object Namespace:
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

  val byPrefix =
    val commonNamespaces = RdfContent :: ApplePodcast :: DublinCore :: Atom :: Podcast :: Spotify :: Media :: CreativeCommons :: Source :: Nil
    commonNamespaces.map( ns => (ns.prefix, ns)).toMap

  private def toBinding( parentScope : NamespaceBinding, list : List[Namespace] ) : NamespaceBinding =
    list match
      case head :: tail => toBinding(new NamespaceBinding(head.prefix, head.uri, parentScope), tail)
      case Nil          => parentScope
  def toBinding( namespaces : List[Namespace]) : NamespaceBinding = toBinding(TopScope, namespaces)
case class Namespace(prefix : String, uri : String)

