import audiofluidity.rss.*
import java.time.*
import scala.collection.*

object FancierExample:
  given Itemable[MyBlog.Post] with
    extension (post : MyBlog.Post)
      def toItem : Element.Item =
        val pubDate : Option[ZonedDateTime] = Some(zonedDateTime(post.published))
        val standardItem =
          Element.Item.create (
            title = post.title,
            linkUrl = post.permalink,
            description = post.desc,
            author = MyBlog.myEmail,
            pubDate = pubDate
          )
        val content = Element.Content.Encoded(text=post.text)
        val creator = Element.DublinCore.Creator(creator=MyBlog.myName)
        standardItem.withExtra(creator).withExtra(content)


  val atomLinkChannelExtra =
    Element.Atom.Link(
      href = MyBlog.mainUrl,
      rel = Some(Element.Atom.LinkRelation.self),
      `type` = Some("application/rss+xml")
    )

  val channel = Element.Channel.create (
    title = "My blog's RSS feed!",
    linkUrl = MyBlog.mainUrl,
    description = "This blog will blow your mind. Or your chance.",
    items = posts
  ).withExtra(atomLinkChannelExtra)

//  We could, probably should, explicitly specify our Namespaces, as in the commented out code
//  below. But the library will recognize usual prefixes for a few common Namespaces and
//  automatically provide those NamespaceBindings. So, let's use the simpler variant now
//  uncommented.
//
//  val rss =
//    Element.Rss(channel)
//      .overNamespaces(Namespace.DublinCore :: Namespace.RdfContent :: Namespace.Atom :: Nil)

  val rss = Element.Rss(channel)

  import scala.xml.{Node, NodeSeq, Elem,Text}
  import scala.xml.transform.{RewriteRule,RuleTransformer}

  val emptyTextNode = Text("")

  // see https://github.com/scala/scala-xml/wiki/Getting-started
  val dropAuthorAndUntitledTitles : Node => Node = (n : Node) => {
    def noKids(elem: Elem) =
      elem.child.isEmpty || elem.child.length == 1 && elem.child(0) == emptyTextNode

    val rewriteRule = new RewriteRule:
      override def transform(n: Node): Seq[Node] =
        n match {
          case elem: Elem if elem.label == "author" => NodeSeq.Empty
          case elem: Elem if elem.label == "title" && noKids(elem) => NodeSeq.Empty
          case n => n
        }
    val transform = new RuleTransformer(rewriteRule)
    transform(n)
  }

  @main def fancy_go() =
    val spec = Element.ToXml.Spec.Default.copy( postprocessor=dropAuthorAndUntitledTitles )
    println( rss.asXmlText( spec ) )

