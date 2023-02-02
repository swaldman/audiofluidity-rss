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
    items = posts.toSeq.map( _.toItem) // toSeq is important here! (why?)
  ).withExtra(atomLinkChannelExtra)

  val rss =
    Element.Rss(channel)
      .overNamespaces(Namespace.DublinCore :: Namespace.RdfContent :: Namespace.Atom :: Nil)

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
    println( rss.asXmlText(transformer = dropAuthorAndUntitledTitles) )

