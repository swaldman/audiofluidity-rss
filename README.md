# audiofluidity-rss

A simple Scala API for generating RSS, for general websites as well as for podcasts.

Support is present for most common RSS extensions, including Apple Podcast "itunes" elements.

## Quickstart

Suppose we have a "blog" defined like this:

```scala
//> using scala "3.2.1"
//> using lib "com.mchange::audiofluidity-rss::0.0.1-SNAPSHOT"

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
```

You can generate simple XML for it like this:

```scala
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
    items = posts.toSeq.map( _.toItem) // toSeq is important here! (why?)
  )

  val rssFeed = Element.Rss(channel)

  @main def simple_go() : Unit =
    println(rssFeed.asXmlText)
```

You can run this from the example directory of this repository:

```sh
$ scala-cli . --interactive        
```

And whee!

```xml
<?xml version='1.0' encoding='UTF-8'?>
<rss version="2.0">
  <channel>
    <title>My blog's RSS feed!</title>
    <link>https://myblog.dev.null/</link>
    <description><![CDATA[This blog will blow your mind. Or your chance.]]></description>
    <docs>https://cyber.harvard.edu/rss/rss.html</docs>
    <item>
      <pubDate>Fri, 10 Feb 2023 13:48:58 -0500</pubDate>
      <author>nospam@dev.null (Arthur Q. Author)</author>
      <description><![CDATA[This is an untitled post.]]></description>
      <link>https://myblog.dev.null//entry/1676054938281.html</link>
      <title></title>
    </item>
    <item>
      <pubDate>Mon, 30 Jan 2023 00:02:18 -0500</pubDate>
      <author>nospam@dev.null (Arthur Q. Author)</author>
      <description><![CDATA[In which I gloat.]]></description>
      <link>https://myblog.dev.null//entry/1675054938281.html</link>
      <title>Pulitzer!</title>
    </item>
    <item>
      <pubDate>Fri, 27 Jan 2023 00:01:04 -0500</pubDate>
      <author>nospam@dev.null (Arthur Q. Author)</author>
      <description><![CDATA[In which I worry.]]></description>
      <link>https://myblog.dev.null//entry/1674795664978.html</link>
      <title>Is this on?</title>
    </item>
    <item>
      <pubDate>Sun, 22 Jan 2023 23:58:43 -0500</pubDate>
      <author>nospam@dev.null (Arthur Q. Author)</author>
      <description><![CDATA[First post!]]></description>
      <link>https://myblog.dev.null//entry/1674449923644.html</link>
      <title>Hello!</title>
    </item>
  </channel>
</rss>
```

## Features

_audiofluidity-rss_ defines lots of not-standard-RSS elements that are commonly
mixed into RSS feeds. For example...
 * You might wish to mix-in non-RSS-standard tags
   * You might want to use `<dc:creator>` elements for an author's name, rather 
     than the e-mail address required in the `<author>` tag by the RSS standard
   * You might wish to include full-text content, rather than just the RSS-standard
     `<description>`, in your feed items. A common way to do this is with RDF-defined
     `<content:encoded>` tags.
   * You might wish to add an `<atom:link>` element to your channel item indicating
     the home page of the blog tht the feed represents
   * If you do any or all of these things, your RSS tag should properly bind
     the right XML namespaces to those `dc`/`content`/`atom` prefixes.
 * Defying the RSS standard, you might wish to drop required `<author>` tags (because
   you don't want to emit e-mails, real or fake), or to drop the required `<title>` element
   for untitled posts (rather than including an empty title).

_audiofluidity_rss_ supports mixing-in nonstandard elements (and offers definitions of
lots of common choices), defining RSS-tag namespaces, and inserting post-processing
into the XML-generation process to [drop or rewrite](https://github.com/scala/scala-xml/wiki/Getting-started)
elements.

For an example, please see `examples/FancierExample.scala`. You can run it in
the examples dir with

```sh
$ scala-cli . --interactive        
```


## Limitations

Although this library defines an informal AST for RSS, for now it only
generates XML, it does not consume and parse it back. 

Maybe someday if there's interest.

## License

_audiofluidity-rss_ was revised from a library internal to [_audiofluidity_](https://github.com/swaldman/audiofluidity),
a podcast-specific static-site generator.

However, this library is now offered independently, under Apache 2.0 terms. Please see
[LICENSE](LICENSE).

(The main _audiofluidity_ application is a GPLv3 project.)


## Some useful RSS resources

More docs soon, I hope. But for now, I want to
bookmark some useful RSS resources:

- [Really Simple Syndication Best Practices Profile](https://www.rssboard.org/rss-profile)
- [RSS 2.0.1 Specification](https://www.rssboard.org/rss-2-0-1)
- [RSS Mime Type](https://www.rssboard.org/rss-mime-type-application.txt)
- [RSS Validator](https://www.rssboard.org/rss-validator/)
- [Excerpt from O'Reilly book by Ben Hammersley](https://www.oreilly.com/library/view/developing-feeds-with/0596008813/ch04s02.html)
- [DublinCore (`dc`) specification](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/)
- [Podcast RSS resources at audiofluidity main](https://github.com/swaldman/audiofluidity#podcast-rss)
- [Atom specification](https://datatracker.ietf.org/doc/html/rfc4287)

See also the podcast-centric RSS resource list in the main [_audiofluidity_ README.md](https://github.com/swaldman/audiofluidity#developer-resources) 