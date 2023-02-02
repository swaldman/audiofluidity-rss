//> using scala "3.2.1"
//> using lib "com.mchange::audiofluidity-rss::0.0.2"

import audiofluidity.rss.*
import java.time.*
import scala.collection.*

object MyBlog {
  val myName = "Arthur Q. Author"
  val myEmail = s"nospam@dev.null (${myName})"
  val mainUrl = "https://myblog.dev.null/"

  case class Post(title: String, desc: String, text : String, published : Long):
    def permalink = s"${mainUrl}/entry/${published}.html"
}

// reverse chronological
given Ordering[MyBlog.Post]
  = Ordering.by((p : MyBlog.Post) => (p.published, p.title, p.desc, p.text)).reverse

val posts = immutable.SortedSet (
  MyBlog.Post("Hello!", "First post!", "Some words I write.", 1674449923644),
  MyBlog.Post("Is this on?", "In which I worry.", "Why was my post not greeted with adulation?", 1674795664978),
  MyBlog.Post("Pulitzer!", "In which I gloat.", "Finally 'Hello !' received the recognition it deserves.", 1675054938281),
  MyBlog.Post("", "This is an untitled post.", "I've got nothing to say but it's okay.", 1676054938281),
)

def zonedDateTime( epochMilli : Long ) =
  Instant.ofEpochMilli(epochMilli).atZone(ZoneId.systemDefault())

