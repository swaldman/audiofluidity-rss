import audiofluidity.rss.*
import java.time.*
import scala.collection.*

object SimpleExample:
  given Itemable[MyBlog.Post] with
    extension (post : MyBlog.Post)
      def toItem : Element.Item =
        val pubDate : Option[ZonedDateTime] = Some(zonedDateTime(post.published))
        Element.Item.create (
          title = post.title,
          linkUrl = post.permalink,
          description = post.desc,
          author = MyBlog.myEmail,
          pubDate = pubDate
        )

  val channel = Element.Channel.create (
    title = "My blog's RSS feed!",
    linkUrl = MyBlog.mainUrl,
    description = "This blog will blow your mind. Or your chance.",
    items = posts
  )

  val rssFeed = Element.Rss(channel)

  @main def simple_go() : Unit =
    println(rssFeed.asXmlText)

