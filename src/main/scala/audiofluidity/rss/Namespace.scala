package audiofluidity.rss

import scala.xml.{NamespaceBinding,TopScope}

object Namespace:
  val RdfContent   = Namespace("content", "http://purl.org/rss/1.0/modules/content/")
  val ApplePodcast = Namespace("itunes", "http://www.itunes.com/dtds/podcast-1.0.dtd")
  val DublinCore   = Namespace("dc","http://purl.org/dc/elements/1.1/")
  val Atom         = Namespace("atom","http://www.w3.org/2005/Atom")

  val byPrefix =
    val commonNamespaces = RdfContent :: ApplePodcast :: DublinCore :: Atom :: Nil
    commonNamespaces.map( ns => (ns.prefix, ns)).toMap

  private def toBinding( parentScope : NamespaceBinding, list : List[Namespace] ) : NamespaceBinding =
    list match
      case head :: tail => toBinding(new NamespaceBinding(head.prefix, head.uri, parentScope), tail)
      case Nil          => parentScope
  def toBinding( namespaces : List[Namespace]) : NamespaceBinding = toBinding(TopScope, namespaces)
case class Namespace(prefix : String, uri : String)

