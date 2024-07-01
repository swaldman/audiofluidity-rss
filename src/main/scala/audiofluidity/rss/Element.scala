package audiofluidity.rss

import java.time.{Instant,ZonedDateTime,ZoneId}
import java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME
import scala.collection.{immutable,mutable}
import scala.xml.{Elem, MetaData, Node, NodeSeq, Null, PCData, PrettyPrinter, Text, TopScope, UnprefixedAttribute}
import scala.util.control.NonFatal

import scala.annotation.targetName

import audiofluidity.rss.util.{formatRFC3339ToSecond,formatPubDate}
import audiofluidity.rss.util.defaultNamespaceUri

import com.mchange.conveniences.collection.*

object Element:
    val RssVersion = "2.0"

    private val UTC = ZoneId.of("Z")

    private def elem(prefix : String)(label : String, attributes1 : MetaData, children : Node*) : Elem =
        new Elem(prefix=prefix, label=label, attributes1=attributes1, scope=TopScope, minimizeEmpty=true, children*)

    private def elem(prefix : String)(label : String, children : Node*) : Elem =
        elem(prefix=prefix)( label=label, attributes1=Null, children*)

    private def elem(label : String, attributes1 : MetaData, children : Node*) : Elem =
        elem(null)( label, attributes1, children* )

    private def elem(label : String, children : Node*) : Elem = elem(label, Null, children*)

    enum ValidDay:
        case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

    case class Author(email : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Author]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("author", new Text(this.email))

    case class Category(domain : Option[String], text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Category]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            elem("category", domain.fold( Null )(d => new UnprefixedAttribute("domain", d, Null)), new Text(this.text))

    object Channel:
          case class Spec (
                title              : String,
                linkUrl            : String,
                description        : String,
                language           : Option[LanguageCode]            = None,
                copyright          : Option[String]                  = None,
                managingEditor     : Option[String]                  = None,
                webmaster          : Option[String]                  = None,
                pubDate            : Option[ZonedDateTime]           = None,
                lastBuildDate      : Option[ZonedDateTime]           = None,
                categories         : immutable.Seq[Element.Category] = Nil,
                generator          : Option[String]                  = None,
                cloud              : Option[Element.Cloud]           = None,
                ttlMinutes         : Option[Int]                     = None,
                image              : Option[Element.Image]           = None,
                rating             : Option[String]                  = None,
                textInput          : Option[Element.TextInput]       = None,
                skipHours          : Option[Element.SkipHours]       = None,
                skipDays           : Option[Element.SkipDays]        = None,
          )

          def create[T : Itemable]( spec : Spec, items : Iterable[T]) : Channel =
                create(
                      spec.title,
                      spec.linkUrl,
                      spec.description,
                      items,
                      spec.language,
                      spec.copyright,
                      spec.managingEditor,
                      spec.webmaster,
                      spec.pubDate,
                      spec.lastBuildDate,
                      spec.categories,
                      spec.generator,
                      spec.cloud,
                      spec.ttlMinutes,
                      spec.image,
                      spec.rating,
                      spec.textInput,
                      spec.skipHours,
                      spec.skipDays,
                )

          def create[T : Itemable] (
                title              : String,
                linkUrl            : String,
                description        : String,
                items              : Iterable[T],
                language           : Option[LanguageCode]    = None,
                copyright          : Option[String]          = None,
                managingEditor     : Option[String]          = None,
                webmaster          : Option[String]          = None,
                pubDate            : Option[ZonedDateTime]   = None,
                lastBuildDate      : Option[ZonedDateTime]   = None,
                categories         : immutable.Seq[Category] = Nil,
                generator          : Option[String]          = None,
                cloud              : Option[Cloud]           = None,
                ttlMinutes         : Option[Int]             = None,
                image              : Option[Image]           = None,
                rating             : Option[String]          = None,
                textInput          : Option[TextInput]       = None,
                skipHours          : Option[SkipHours]       = None,
                skipDays           : Option[SkipDays]        = None,
          ) : Channel =
                // where we had convenience circumventions, we have to build our elements
                val elemTitle = Title(title)
                val elemLink  = Link(linkUrl)
                val elemDesc  = Description(description)

                val mbLanguage       = language.map( Language(_) )
                val mbCopyright      = copyright.map( Copyright(_) )
                val mbManagingEditor = managingEditor.map( ManagingEditor(_) )
                val mbWebmaster      = webmaster.map( WebMaster(_) )
                val mbPubDate        = pubDate.map( PubDate(_) )
                val mbLastBuildDate  = lastBuildDate.map( LastBuildDate(_) )
                val mbGenerator      = generator.map( Generator(_) )
                val mbTtl            = ttlMinutes.map( Ttl(_) )
                val mbRating         = rating.map( Rating(_) )

                Channel(
                      title = elemTitle,
                      link = elemLink,
                      description = elemDesc,
                      language = mbLanguage,
                      copyright = mbCopyright,
                      managingEditor = mbManagingEditor,
                      webMaster = mbWebmaster,
                      pubDate = mbPubDate,
                      lastBuildDate = mbLastBuildDate,
                      categories = categories,
                      generator = mbGenerator,
                      docs = Some(Docs()), // use the default docs URL pretty much always
                      cloud = cloud,
                      ttl = mbTtl,
                      image = image,
                      rating = mbRating,
                      textInput = textInput,
                      skipHours = skipHours,
                      skipDays = skipDays,
                      items = items.toSeq.map( _.toItem )
                )
    case class Channel(
        title           : Title,
        link            : Link,
        description     : Description,
        language        : Option[Language] = None,
        copyright       : Option[Copyright] = None,
        managingEditor  : Option[ManagingEditor] = None,
        webMaster       : Option[WebMaster] = None,
        pubDate         : Option[PubDate] = None,
        lastBuildDate   : Option[LastBuildDate] = None,
        categories      : immutable.Seq[Category] = immutable.Seq.empty,
        generator       : Option[Generator] = None,
        docs            : Option[Docs] = Some(Docs()), // use default docs URL
        cloud           : Option[Cloud] = None,
        ttl             : Option[Ttl] = None,
        image           : Option[Image] = None,
        rating          : Option[Rating] = None,
        textInput       : Option[TextInput] = None,
        skipHours       : Option[SkipHours] = None,
        skipDays        : Option[SkipDays] = None,
        items           : immutable.Seq[Item],
        namespaces      : List[Namespace] = Nil,
        reverseExtras   : List[Extra] = Nil,
        extraAttributes : MetaData = Null, 
        asLastParsed    : Option[Elem] = None
    ) extends Element[Channel]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val itemElems = this.items.map(_.toElem)
            val kids : Vector[Elem] =
                  (Vector.empty[Elem] :+ this.title.toElem :+ this.link.toElem :+ this.description.toElem) ++
                  this.language.map(_.toElem) ++ this.copyright.map(_.toElem) ++ this.managingEditor.map(_.toElem) ++
                  this.webMaster.map(_.toElem) ++ this.pubDate.map(_.toElem) ++ this.lastBuildDate.map(_.toElem) ++
                  this.categories.map(_.toElem) ++ this.generator.map(_.toElem) ++ this.docs.map(_.toElem) ++
                  this.cloud.map(_.toElem) ++ this.ttl.map(_.toElem) ++ this.image.map(_.toElem) ++ this.rating.map(_.toElem) ++
                  this.textInput.map(_.toElem) ++ this.skipHours.map(_.toElem) ++ this.skipDays.map(_.toElem) ++ itemElems
            elem("channel", kids : _*)
        override def reorderedChildren(rawChildren : List[Node]) : List[Node] =
          def isItemElem( n : Node ) : Boolean =
            n match
                case elem : Elem => elem.label == "item"
                case _           => false
          val (items, other) = rawChildren.partition( isItemElem )
          other ::: items

    case class Cloud(domain : String, port : Int, path : String, registerProcedure : String, protocol : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Cloud]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val attributes = new UnprefixedAttribute("domain", this.domain,
                new UnprefixedAttribute("port", this.port.toString,
                    new UnprefixedAttribute("path", this.path,
                        new UnprefixedAttribute("registerProcedure", this.registerProcedure,
                            new UnprefixedAttribute("protocol", this.protocol, Null)
                        )
                    )
                )
            )
            elem("cloud", attributes )

    case class Comments(url : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Comments]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("comments", new Text(this.url))

    case class Copyright(notice : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Copyright]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("copyright", new Text(this.notice))

    case class Day(day : ValidDay, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Day]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("day", new Text(this.day.toString))

    case class Description(text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Description]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("description", new PCData(this.text))

    case class Docs(url : String = "https://cyber.harvard.edu/rss/rss.html", namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Docs]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("docs", new Text(this.url))

    case class Enclosure(url : String, length : Long, `type` : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Enclosure]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val attributes = UnprefixedAttribute("url", this.url,
                new UnprefixedAttribute("length", this.length.toString,
                    new UnprefixedAttribute("type", this.`type`, Null)
                )
            )
            elem("enclosure", attributes )

    case class Generator(description : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Generator]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("generator", new Text(this.description))

    case class Guid(isPermalink : Boolean, id : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Guid]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("guid", new UnprefixedAttribute("isPermalink", this.isPermalink.toString, Null), new Text(this.id))

    // should be 1 to 24
    case class Hour(hour : Int, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Hour]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("hour", new Text(this.hour.toString))

    case class Height(pixels : Int, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Height]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("height", new Text(this.pixels.toString))

    case class Image(url : Url, title : Title, link : Link, width : Option[Width], height : Option[Height], description : Option[Description], namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Image]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val reverseKids: List[Elem] =
                this.description.map(_.toElem) ++: this.height.map(_.toElem) ++: this.width.map(_.toElem) ++: (this.link.toElem :: this.title.toElem :: this.url.toElem :: Nil)
            elem("image",reverseKids.reverse : _*)

    object Item:
        def create(
            title       : String,
            linkUrl     : String,
            description : String,
            author      : String,
            categories  : immutable.Seq[Category] = immutable.Seq.empty,
            comments    : Option[String]          = None,
            enclosure   : Option[Enclosure]       = None,
            guid        : Option[Guid]            = None,
            pubDate     : Option[ZonedDateTime]   = None,
            source      : Option[Source]          = None,
        ) : Item = this.apply (
            if title.nonEmpty then Some(Title(title)) else None,
            if linkUrl.nonEmpty then Some(Link(linkUrl)) else None,
            if description.nonEmpty then Some(Description(description)) else None,
            if author.nonEmpty then Some(Author(author)) else None,
            categories,
            comments.map(Comments(_)),
            enclosure,
            guid,
            pubDate.map(PubDate(_)),
            source
        )
    case class Item(
        title           : Option[Title],
        link            : Option[Link],
        description     : Option[Description],
        author          : Option[Author],
        categories      : immutable.Seq[Category] = immutable.Seq.empty,
        comments        : Option[Comments]        = None,
        enclosure       : Option[Enclosure]       = None,
        guid            : Option[Guid]            = None,
        pubDate         : Option[PubDate]         = None,
        source          : Option[Source]          = None,
        namespaces      : List[Namespace]         = Nil,
        reverseExtras   : List[Extra]             = Nil,
        extraAttributes : MetaData                = Null, 
        asLastParsed    : Option[Elem]            = None
    ) extends Element[Item]:
        // from the spec "All elements of an item are optional, however at least one of title or description must be present."
        require( link.nonEmpty || description.nonEmpty, "One of description or link must always be provided. Neither has ben provided." )

        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val kids =
                this.categories.map(_.toElem) ++ this.source.map(_.toElem) ++ this.pubDate.map(_.toElem) ++ this.guid.map(_.toElem) ++ this.enclosure.map(_.toElem) ++
                  this.comments.map(_.toElem) ++ this.author.map( _.toElem ) ++ this.description.map( _.toElem ) ++ this.link.map( _.toElem ) ++ this.title.map( _.toElem )
            elem("item", kids : _*)

    case class Language(code : LanguageCode, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Language]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("language", new Text(this.code.rendered))

    case class LastBuildDate(date : ZonedDateTime, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[LastBuildDate]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val dateStr = formatPubDate(this.date)
            elem("lastBuildDate", new Text(dateStr))

    case class Link(location : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Link]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("link", new Text(this.location))

    case class ManagingEditor(email : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[ManagingEditor]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("managingEditor", new Text(this.email))

    case class Name(text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Name]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("name", new Text(this.text))

    case class PubDate(date : ZonedDateTime, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[PubDate]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val dateStr = formatPubDate(this.date)
            elem("pubDate", new Text(dateStr))

    // this seems widely unutilized, not sure what the contents might look like exactly
    case class Rating(contents : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Rating]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("rating", new Text(this.contents))

    case class Rss( channel : Channel, version : String = RssVersion, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Rss]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem =
            elem("rss", new UnprefixedAttribute("version", RssVersion, Null), this.channel.toElem)

    case class SkipDays(days : immutable.Seq[Day], namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[SkipDays]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("skipDays", this.days.map( _.toElem ) : _*)

    case class SkipHours(hours : immutable.Seq[Hour], namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[SkipHours]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("skipHours", this.hours.map( _.toElem ) : _*)

    case class Source(url : String, title : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Source]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("source", new UnprefixedAttribute("url",this.url,Null), new Text(this.title))

    // mostly unutilized
    case class TextInput(title : Title, description : Description, name : Name, link : Link, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[TextInput]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("textInput", this.title.toElem, this.description.toElem, this.name.toElem, this.link.toElem)

    case class Title(text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Title]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("title", new Text(this.text))

    case class Ttl(minutes : Int, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Ttl]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("ttl", new Text(this.minutes.toString))

    case class Url(location : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Url]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("url", new Text(this.location))

    case class WebMaster(email : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[WebMaster]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("webMaster", new Text(this.email))

    case class Width(pixels : Int, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Width]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("width", new Text(this.pixels.toString))

    object Atom:
        object LinkRelation:
          def lenientParse( string : String ) : Option[LinkRelation] = LinkRelation.values.find( _.toString.equalsIgnoreCase( string.trim ) )
        enum LinkRelation:
            case alternate, related, self, enclosure, via

        // for now, probably forever
        // see https://datatracker.ietf.org/doc/html/rfc3987
        // via https://datatracker.ietf.org/doc/html/rfc4287#section-4.2.7.2
        opaque type Iri = String
        def toIri( s : String ) : Iri = s

        object Link extends Parser[Link](Some(Namespace.Atom),"link"):
          override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Link)] ) =
            val warnings = Vector.newBuilder[String]
            val used = Vector.newBuilder[Elem]
            val reverseExtras = allChildElemsAsReverseExtras(warnings)(elem)

            var extraAttributes = attributesExcept("href","rel","type","hreflang","title","length")( elem.attributes )

            val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
            val mbHref = getAttr(elem.attributes)("href")
            val mbRel = getAttr(elem.attributes)("rel").map( raw => LinkRelation.lenientParse(raw).getOrElse( toIri(raw) ) )
            val mbType = getAttr(elem.attributes)("type")
            val mbHrefLang =
              getAttr(elem.attributes)("hreflang").flatMap: raw =>
                LanguageCode.byRendered.get(raw).orElse:
                  warnings += "Could not parse atom:link hreflang '${raw}' to a known language code. Omitting!"
                  extraAttributes = prependAttribute("hreflang",raw,extraAttributes)
                  None
            val mbTitle = getAttr(elem.attributes)("title")
            val mbLength =
              getAttr(elem.attributes)("length").flatMap: raw =>
                try Some(raw.toLong)
                catch
                  case NonFatal(t) =>
                    warnings += s"Failed to parse atom:link length '${raw}' to a valid long integer. Omitting! (${t})"
                    extraAttributes = prependAttribute("length",raw,extraAttributes)
                    None
            mbHref match
              case Some(href) =>
                ( warnings.result, Some( ( elem, Link( href, mbRel, mbType, mbHrefLang, mbTitle, mbLength, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
              case None =>
                warnings += "Invalid atom:link element, missing href attribute. Skipping!"
                ( warnings.result, None )
        case class Link(
          href            : String,
          rel             : Option[LinkRelation | Iri] = None,
          `type`          : Option[String]             = None,
          hreflang        : Option[LanguageCode]       = None,
          title           : Option[String]             = None,
          length          : Option[Long]               = None,
          namespaces      : List[Namespace]            = Nil,
          reverseExtras   : List[Extra]                = Nil,
          extraAttributes : MetaData                   = Null,
          asLastParsed    : Option[Elem]               = None
        ) extends Element[Link]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem: Elem =
                val attributeTups =
                    List(Tuple2("href", this.href)) ++
                      this.rel.map(rel => Tuple2("rel", rel.toString)) ++
                      this.`type`.map(tpe => Tuple2("type", tpe)) ++
                      this.hreflang.map(hl => Tuple2("hreflang", hl.rendered)) ++
                      this.title.map(t => Tuple2("title", t)) ++
                      this.length.map(l => Tuple2("length", l.toString))

                val attributes = attributeTups.foldLeft(Null: MetaData) { (accum, next) =>
                    new UnprefixedAttribute(next(0), next(1), accum)
                }
                Elem(prefix = "atom", label = "link", attributes = attributes, scope = TopScope, minimizeEmpty = true )
        object Published extends Parser[Published](Some(Namespace.Atom),"published"):
            override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Published)] ) =
              val warnings = Vector.newBuilder[String]
              val reverseExtras = allChildElemsAsReverseExtras(warnings)(elem)
              val extraAttributes = elem.attributes
              val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
              try
                val zdt = ZonedDateTime.parse( elem.text )
                ( warnings.result, Some( ( elem, Published(zdt, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
              catch
                case NonFatal(t) =>
                  warnings += s"Could not parse text '${elem.text}' as timestamp: $t"
                  ( warnings.result, None )
            def apply( instant : Instant ) : Published = apply( instant.atZone(UTC) )
        case class Published( zdt : ZonedDateTime, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None ) extends Element[Published]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = new Elem(prefix="atom", label="published", attributes1=Null, scope=TopScope, minimizeEmpty=true, new Text(formatRFC3339ToSecond(zdt)))
        object Summary extends Parser[Summary](Some(Namespace.Atom),"summary"):
          override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Summary)] ) =
            val warnings = Vector.newBuilder[String]
            val reverseExtras = allChildElemsAsReverseExtras(warnings)(elem)
            val extraAttributes = elem.attributes
            val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
            ( warnings.result, Some( ( elem, Summary( elem.text , reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
        case class Summary( text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None ) extends Element[Summary]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = new Elem(prefix="atom", label="summary", attributes1=Null, scope=TopScope, minimizeEmpty=true, new PCData(text))
        object Title extends Parser[Title](Some(Namespace.Atom),"title"):
          override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Title)] ) =
            val warnings = Vector.newBuilder[String]
            val reverseExtras = allChildElemsAsReverseExtras(warnings)(elem)
            val extraAttributes = elem.attributes
            val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
            ( warnings.result, Some( ( elem, Title( elem.text , reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
        case class Title( text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None ) extends Element[Title]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = new Elem(prefix="atom", label="title", attributes1=Null, scope=TopScope, minimizeEmpty=true, new PCData(text))
        object Updated extends Parser[Updated](Some(Namespace.Atom),"updated"):
            override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Updated)] ) =
              val warnings = Vector.newBuilder[String]
              val reverseExtras = allChildElemsAsReverseExtras(warnings)(elem)
              val extraAttributes = elem.attributes
              val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
              try
                val zdt = ZonedDateTime.parse( elem.text )
                ( warnings.result, Some( ( elem, Updated(zdt, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
              catch
                case NonFatal(t) =>
                  warnings += s"Could not parse text '${elem.text}' as timestamp: $t"
                  ( warnings.result, None )
            def apply( instant : Instant ) : Updated = apply( instant.atZone(UTC) )
        case class Updated( zdt : ZonedDateTime, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None ) extends Element[Updated]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = new Elem(prefix="atom", label="updated", attributes1=Null, scope=TopScope, minimizeEmpty=true, new Text(formatRFC3339ToSecond(zdt)))

    object Content:
        case class Encoded(text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Encoded]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = new Elem(prefix="content", label="encoded", attributes1=Null, scope=TopScope, minimizeEmpty=true, new PCData(this.text))

    object DublinCore:
        object Creator extends Parser[Creator](Some(Namespace.DublinCore),"creator"):
          override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Creator)] ) =
            val warnings = Vector.newBuilder[String]
            val reverseExtras = allChildElemsAsReverseExtras(warnings)(elem)
            val extraAttributes = elem.attributes
            val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
            ( warnings.result, Some( ( elem, Creator( elem.text.trim, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
        case class Creator(creator : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Creator]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem: Elem =
                Elem(prefix = "dc", label = "creator", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new PCData(this.creator))

    object Source:
        case class Account(service : String, account : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Account]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem: Elem =
                Elem(prefix = "source", label = "account", attributes = new UnprefixedAttribute("service", this.service, Null), scope = TopScope, minimizeEmpty = true, child = new Text(this.account))

        case class Blogroll(blogroll : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Blogroll]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem: Elem =
                Elem(prefix = "source", label = "blogroll", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(this.blogroll))

    // see https://www.rssboard.org/comment-api
    object WellFormedWeb:
      case class Comment( commentAcceptingUrl : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Comment]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem: Elem =
                Elem(prefix = "wfw", label = "comment", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(commentAcceptingUrl))

      case class CommentRss( rssUrl : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[CommentRss]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem: Elem =
                Elem(prefix = "wfw", label = "commentRss", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(rssUrl))

    object Iffy:
      object Completeness:
        object Value:
          def lenientParse( string : String ) : Option[Value] = Value.values.find( _.toString.equalsIgnoreCase( string.trim ) )
        enum Value:
          case Ping, Metadata, Content, Media
      case class Completeness( value : Completeness.Value, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Completeness]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
          Elem(prefix = "iffy", label = "completeness", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(value.toString))
      object Diff extends Parser[Diff](Some(Namespace.Iffy),"diff"):
        override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Diff)] ) =
          val warnings = Vector.newBuilder[String]
          val reverseExtras = allChildElemsAsReverseExtras(warnings)(elem)
          val extraAttributes = elem.attributes
          val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
          ( warnings.result, Some( ( elem, Diff( elem.text.trim, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
      case class Diff( url : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Diff]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
          Elem(prefix = "iffy", label = "diff", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(url))
      object HintAnnounce extends Parser[HintAnnounce](Some(Namespace.Iffy),"hint-announce"):
        object Policy:
          def lenientParse( string : String ) : Option[Policy] = Policy.values.find( _.toString.equalsIgnoreCase( string.trim ) )
        enum Policy:
          case Always, Never, Piggyback

        override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,HintAnnounce)] ) =
          val warnings = Vector.newBuilder[String]
          val used = Vector.newBuilder[Elem]
          val mbPolicy = Iffy.Policy.extractFromChildrenWarnUseFirst( warnings, used )( elem )
          val mbRestriction = Iffy.Restriction.extractFromChildrenWarnUseFirst( warnings, used )( elem )
          val reverseExtras = childElemsAsReverseExtrasExcept(warnings)(used.result)(elem)
          val extraAttributes = elem.attributes
          val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
          mbPolicy match
            case Some( policy ) =>
              if Policy.lenientParse( policy.value ).isEmpty then
                warnings += s"Found unknown iffy:int-announce policy: ${policy.value}"
              (warnings.result, Some((elem, HintAnnounce(policy,mbRestriction, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed))))
            case None =>
              warnings += "No policy element found while parsing iffy:hint-announce. Skipping."
              (warnings.result, None)
        end _fromChecked
        def apply( policy : Iffy.HintAnnounce.Policy, restriction : Option[Restriction] ) : HintAnnounce = apply( Iffy.Policy(policy.toString), restriction )
        def apply( policy : Iffy.HintAnnounce.Policy ) : HintAnnounce = apply( Iffy.Policy(policy.toString) )
      end HintAnnounce 
      case class HintAnnounce( policy : Iffy.Policy, restriction : Option[Restriction] = None, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[HintAnnounce]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            Elem(prefix = "iffy", label = "hint-announce", attributes = Null, scope = TopScope, minimizeEmpty = true, child = (Seq(policy.toElem) ++ restriction.map(_.toElem))*)
      object Initial extends Parser[Initial](Some(Namespace.Iffy),"initial"):
        override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Initial)] ) =
          val warnings = Vector.newBuilder[String]
          val used = Vector.newBuilder[Elem]
          val title = Atom.Title.extractFromChildrenWarnUseFirst(warnings,used)(elem)
          val link = Atom.Link.extractFromChildrenWarnUseFirst(warnings,used)(elem)
          val guid = Iffy.Uid.extractFromChildrenWarnUseFirst(warnings,used)(elem)
          val published = Atom.Published.extractFromChildrenWarnUseFirst(warnings,used)(elem)
          val creators = DublinCore.Creator.extractFromChildrenWarnUseAll(warnings,used)(elem)
          val reverseExtras = childElemsAsReverseExtrasExcept(warnings)(used.result)( elem )
          val extraAttributes = elem.attributes
          val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
          ( warnings.result, Some( ( elem, Initial( title, link, guid, published, creators, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
      case class Initial(
        title : Option[Atom.Title]         = None,
        link : Option[Atom.Link]           = None,
        guid : Option[Uid]                 = None,
        published : Option[Atom.Published] = None,
        creators : Seq[DublinCore.Creator] = Nil,
        namespaces : List[Namespace]       = Nil,
        reverseExtras : List[Extra]        = Nil,
        extraAttributes : MetaData         = Null,
        asLastParsed : Option[Elem]        = None
      ) extends Element[Initial]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem(prefix="iffy")(label="initial", (title++link++guid++published++creators).toSeq.map(_.toElem)*)
      object Policy extends Parser[Policy](Some(Namespace.Iffy),"policy"):
        override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Policy)] ) =
          val warnings = Vector.newBuilder[String]
          val reverseExtras = allChildElemsAsReverseExtras(warnings)(elem)
          val extraAttributes = elem.attributes
          val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
          ( warnings.result, Some( ( elem, Policy( elem.text.trim, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
      case class Policy( value : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Policy]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            Elem(prefix = "iffy", label = "policy", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(value))
      object Provenance extends Parser[Provenance](Some(Namespace.Iffy),"provenance"):
        object Shape:
          def lenientParse( string : String ) : Option[Shape] = Shape.values.find( _.toString.equalsIgnoreCase( string.trim ) )
        enum Shape:
          case sequence, merge
        override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem, Provenance)] ) =
          val warnings = Vector.newBuilder[String]
          val used = Vector.newBuilder[Elem]

          var extraAttributes = attributesExcept("shape")(elem.attributes)

          val linksAndProvenances : Seq[Atom.Link|Iffy.Provenance] =
            elem.child.collect { node =>
              node match
                case al : Elem if Atom.Link.check(al)       => Atom.Link.maybeFromWarnUse(warnings,used)(al)
                case ip : Elem if Iffy.Provenance.check(ip) => Iffy.Provenance.maybeFromWarnUse(warnings,used)(ip)
                case _                                      => None
            }.flatten
          val shape =
            getAttr(elem.attributes)("shape").flatMap: raw =>
              Shape.lenientParse(raw).orElse:
                extraAttributes = prependAttribute("shape",raw,extraAttributes)
                warnings += "Found unexpected iffy:provenance shape value: ${str}. Skipping."
                None
          val reverseExtras = childElemsAsReverseExtrasExcept(warnings)( used.result )(elem)
          val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
          ( warnings.result, Some( ( elem, Provenance( linksAndProvenances, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
      case class Provenance(
        linksAndProvenances : Seq[Atom.Link|Iffy.Provenance],
        shape : Option[Provenance.Shape] = None,
        namespaces : List[Namespace] = Nil,
        reverseExtras : List[Extra] = Nil,
        extraAttributes : MetaData = Null,
        asLastParsed : Option[Elem] = None
      ) extends Element[Provenance]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val attributes = shape.fold(Null)( shape => new UnprefixedAttribute("shape", shape.toString, Null) )
            Elem(prefix = "iffy", label = "provenance", attributes = attributes, scope = TopScope, minimizeEmpty = true, child = linksAndProvenances.map(_.toElem)*)
      object Restriction extends Parser[Restriction](Some(Namespace.Iffy),"restriction"):
        override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Restriction)] ) =
          val warnings = Vector.newBuilder[String]
          val reverseExtras = allChildElemsAsReverseExtras(warnings)( elem )
          val extraAttributes = elem.attributes
          val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
          ( warnings.result, Some( ( elem, Restriction( reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
      case class Restriction( namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Restriction]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            Elem(prefix = "iffy", label = "restriction", attributes = Null, scope = TopScope, minimizeEmpty = true)
      object Revision extends Parser[Revision](Some(Namespace.Iffy),"revision"):
        override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Revision)] ) =
          val warnings = Vector.newBuilder[String]
          val reverseExtras = allChildElemsAsReverseExtras(warnings)(elem)
          val extraAttributes = elem.attributes
          val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
          ( warnings.result, Some( ( elem, Revision( elem.text.trim, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
      case class Revision( url : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Revision]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
          Elem(prefix = "iffy", label = "revision", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(url))
      object Synthetic extends Parser[Synthetic](Some(Namespace.Iffy),"synthetic"):
        object KnownType:
          def lenientParse( string : String ) : Option[KnownType] = KnownType.values.find( _.toString.equalsIgnoreCase( string.trim ) )
        enum KnownType:
          case ItemUpdateFeed, UpdateAnnouncement, UpdateCumulation
        override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Synthetic)] ) =
          val warnings = Vector.newBuilder[String]
          val used = Vector.newBuilder[Elem]
          val `type` = Iffy.Type.extractFromChildrenWarnUseFirst(warnings,used)(elem)
          `type` match
            case Some(tpe) if KnownType.lenientParse(tpe.value).isEmpty =>
              warnings += s"iffy:synthetic type '$tpe' is not a type we know. iffy:synthetic does accept unknown types, so we have accepted '${tpe}'."
            case _ => /* ignore */
          val reverseExtras = childElemsAsReverseExtrasExcept(warnings)( used.result )(elem)
          val extraAttributes = elem.attributes
          val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
          ( warnings.result, Some( ( elem, Synthetic( `type`, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
      case class Synthetic( `type` : Option[Type] = None, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Synthetic]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            Elem(prefix = "iffy", label = "synthetic", attributes = Null, scope = TopScope, minimizeEmpty = true, child = `type`.map(_.toElem).toSeq* )
      object Type extends Parser[Type](Some(Namespace.Iffy),"type"):
        override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Type)] ) =
          val warnings = Vector.newBuilder[String]
          val reverseExtras = allChildElemsAsReverseExtras(warnings)(elem)
          val extraAttributes = elem.attributes
          val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
          ( warnings.result, Some( ( elem, Type( elem.text.trim, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
      case class Type( value : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Type]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            Elem(prefix = "iffy", label = "type", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(value))
      object Uid extends Parser[Uid](Some(Namespace.Iffy),"id"):
        override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Uid)] ) =
          val warnings = Vector.newBuilder[String]
          val reverseExtras = allChildElemsAsReverseExtras(warnings)(elem)
          val extraAttributes = elem.attributes
          val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
          ( warnings.result, Some( ( elem, Uid( elem.text.trim, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
      case class Uid( value : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Uid]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
          Elem(prefix = "iffy", label = "id", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(value))
      object Update extends Parser[Update](Some(Namespace.Iffy),"update"):
        override def _fromChecked( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,Update)] ) =
          val warnings = Vector.newBuilder[String]
          val used = Vector.newBuilder[Elem]
          val updated = Atom.Updated.extractFromChildrenWarnUseFirst(warnings,used)(elem)
          val summary = Atom.Summary.extractFromChildrenWarnUseFirst(warnings,used)(elem)
          val revision = Iffy.Revision.extractFromChildrenWarnUseFirst(warnings,used)(elem)
          val diff = Iffy.Diff.extractFromChildrenWarnUseFirst(warnings,used)(elem)
          val title = Atom.Title.extractFromChildrenWarnUseFirst(warnings,used)(elem)
          val creators = DublinCore.Creator.extractFromChildrenWarnUseAll(warnings,used)(elem)
          val initial = Iffy.Initial.extractFromChildrenWarnUseFirst(warnings,used)(elem)
          updated match
            case None =>
              warnings += "Required atom:updated element is missing from iffy.updated. Skipping."
              ( warnings.result, None )
            case Some(u) =>
              val reverseExtras = childElemsAsReverseExtrasExcept(warnings)( used.result )(elem)
              val extraAttributes = elem.attributes
              val asLastParsed = if in(pconfig.retainParsed) then Some(elem) else None
              ( warnings.result, Some( ( elem, Update( u, summary, revision, diff, title, creators, initial, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) ) )
      case class Update(
        updated         : Atom.Updated,
        summary         : Option[Atom.Summary]    = None,
        revision        : Option[Revision]        = None,
        diff            : Option[Diff]            = None,
        title           : Option[Atom.Title]      = None,
        creators        : Seq[DublinCore.Creator] = Nil,
        initial         : Option[Initial]         = None,
        namespaces      : List[Namespace]         = Nil,
        reverseExtras   : List[Extra]             = Nil,
        extraAttributes : MetaData                = Null,
        asLastParsed    : Option[Elem]            = None
      ) extends Element[Update]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem(prefix="iffy")(label="update", (Seq(updated)++summary++revision++diff++creators++initial).map(_.toElem)*)
      case class UpdateHistory(
        updates : Seq[Update],
        initial : Option[Initial],
        namespaces : List[Namespace] = Nil,
        reverseExtras : List[Extra]  = Nil,
        extraAttributes : MetaData   = Null,
        asLastParsed : Option[Elem]  = None
      ) extends Element[UpdateHistory]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem(prefix="iffy")(label="update-history", (updates++initial).map(_.toElem)* )

    // Apple-defined itunes elements
    private def ielem(label : String, attributes1 : MetaData, children : Node*) : Elem =
        new Elem(prefix="itunes", label=label, attributes1=attributes1, scope=TopScope, minimizeEmpty=true, children : _*)
    private def ielem(label : String, children : Node*) : Elem = ielem(label, Null, children : _*)

    object Itunes:
        enum ValidPodcastType:
            case episodic, serial
        enum ValidEpisodeType:
            case full, trailer, bonus

        case class Author(fullName : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Author]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("author", new Text(this.fullName))

        // should always contain "Yes"
        case class Block(namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Block]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("block", new Text("Yes"))

        case class Category(text : String, subcategory : Option[Itunes.Category] = None, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Category]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem: Elem =
                ielem("category", new UnprefixedAttribute("text", this.text, Null), this.subcategory.map(_.toElem).toSeq : _*)

        // should always contain "Yes"
        case class Complete(namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Complete]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("complete", new Text("Yes"))

        case class Duration(seconds : Long, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Duration]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("duration", new Text(this.seconds.toString))

        case class Email(email : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Email]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("email", new Text(this.email))

        case class Episode(number : Int, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Episode]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("episode", new Text(this.number.toString))

        case class EpisodeType(validEpisodeType : Itunes.ValidEpisodeType, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[EpisodeType]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("episodeType", new Text(this.validEpisodeType.toString))

        case class Explicit(isExplicit : Boolean, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Explicit]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("explicit", new Text(this.isExplicit.toString))

        case class Image(href : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Image]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem: Elem = ielem("image", new UnprefixedAttribute("href", this.href, Null) )

        case class Keywords(keywords : immutable.Seq[String], namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Keywords]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("keywords", new Text(this.keywords.mkString(",")))

        case class Name(name : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Name]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("name", new Text(this.name))

        case class NewFeedUrl(location : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[NewFeedUrl]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("new-feed-url", new Text(this.location))

        case class Owner(name : Itunes.Name, email : Itunes.Email, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Owner]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("owner", this.name.toElem, this.email.toElem)

        case class Season(number : Int, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Season]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("season", new Text(this.number.toString))

        case class Subtitle(text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Subtitle]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("subtitle", new Text(this.text))

        case class Summary(text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Summary]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("summary", new Text(this.text))

        case class Title(title : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Title]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("title", new Text(this.title))

        case class Type(validType : Itunes.ValidPodcastType, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Type]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("type", new Text(this.validType.toString))

    object Extra:
      def apply( sourceElement : Element[?] ) : Extra = Extra( Some(sourceElement), sourceElement.toElem )
      def apply( unpairedElem  : Elem       ) : Extra = Extra( None, unpairedElem )
    case class Extra( mbSourceElement : Option[Element[?]], elem : Elem )

    object Kind:
      def forElem( elem : Elem ) : Option[Kind] =
        try Some(Kind(Namespace.guessForElem(elem), elem.label))
        catch
          case _ : IncompleteNamespace => None
    case class Kind( namespace : Option[Namespace], label : String )

    object Kinds:
      //val DefaultRetainParsed = Set( Iffy.Restriction.kind )
      val None = Set.empty[Kind]
      object All
      def contains( kinds : Kinds, kind : Kind ) : Boolean =
        kinds match
          case Kinds.All => true
          case set : Set[Kind] => set.contains(kind)

    type Kinds = Set[Kind] | Kinds.All.type


    trait Parser[T <: Element[T]]( val namespace : Option[Namespace], val label : String ) extends ParserUtils:
      def _fromChecked( elem : Elem )(using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,T)] )

      final def fromChecked( elem : Elem )(using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,T)] ) =
        def preempt = pconfig.preempts.find(this).map( _( elem, pconfig ) )
        def main = Some( _fromChecked( elem ) )
        def fallback = pconfig.fallbacks.find(this).map( _( elem, pconfig ) )

        def attempt( wcollector : mutable.Growable[String] )( step : Option[ ( Seq[String], Option[(Elem, T)] ) ] ) : Option[(Elem, T)] =
          step match
            case Some( warnings, opt ) =>
              wcollector ++= warnings
              opt
            case None => None


        val warnings = Vector.newBuilder[String]
        val result = attempt(warnings)(preempt) orElse attempt(warnings)(main) orElse attempt(warnings)(fallback)

        ( warnings.result, result )

      def check( elem : Elem ) : Boolean =
        elem.label == label && namespace.fold( defaultNamespaceUri(elem.scope) == None )( ns => ns.belongsLenient(elem) )
      def maybeFrom( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Option[(Elem,T)] ) =
        if check( elem ) then fromChecked( elem ) else ( Nil, None )
      def extractFromChildren( parent : Elem )( using pconfig : Parser.Config ) : (Seq[String], Seq[(Elem,T)]) =
        parent.child.foldLeft( (Vector.empty[String], Vector.empty[(Elem,T)] ) ): ( accum, next ) =>
          next match
            case elem : Elem  =>
              val (newWarnings, mbParsed)      = maybeFrom( elem )
              val (oldWarnings, alreadyParsed) = accum // XXX: we extract rather than use accum(0), accum(1) 'cuz weird compiler errors, Scala 3.3.3
              ( oldWarnings ++ newWarnings, alreadyParsed ++ mbParsed )
            case other =>
              accum
      def extractFromChildrenAndWarn(warnings : mutable.Growable[String])( parent : Elem )( using pconfig : Parser.Config ) : Seq[(Elem,T)] =
        val ( ws, ts ) = extractFromChildren( parent )
        warnings ++= ws
        ts
      def maybeFromAndWarn(warnings : mutable.Growable[String])( elem : Elem )( using pconfig : Parser.Config ) : Option[(Elem,T)] =
        val ( ws, mbt ) = maybeFrom( elem )
        warnings ++= ws
        mbt
      def fromCheckedAndWarn(warnings : mutable.Growable[String])( elem : Elem )( using pconfig : Parser.Config ) : Option[(Elem,T)] =
        val ( ws, mbt ) = fromChecked( elem )
        warnings ++= ws
        mbt
      def useAll( used : mutable.Growable[Elem] )( tups : Seq[(Elem,T)] ) : Seq[T] =
        val (elems, ts) = tups.unzip
        used ++= elems
        ts
      def useFirst( used : mutable.Growable[Elem] )( tups : Seq[(Elem,T)] ) : Option[T] =
        tups.headOption.map: head =>
          used += head._1
          head._2
      def useMaybe( used : mutable.Growable[Elem] )( mbTup : Option[(Elem,T)] ) : Option[T] =
        mbTup.map: tup =>
          used += tup._1
          tup._2
      def extractFromChildrenWarnUseAll( warnings : mutable.Growable[String], used : mutable.Growable[Elem] )( parent : Elem )( using pconfig : Parser.Config ) : Seq[T] =
        useAll(used)(extractFromChildrenAndWarn(warnings)(parent))
      def extractFromChildrenWarnUseFirst( warnings : mutable.Growable[String], used : mutable.Growable[Elem] )( parent : Elem )( using pconfig : Parser.Config ) : Option[T] =
        useFirst(used)(extractFromChildrenAndWarn(warnings)(parent))
      def maybeFromWarnUse(warnings : mutable.Growable[String], used : mutable.Growable[Elem])( elem : Elem )( using pconfig : Parser.Config ) : Option[T] =
        useMaybe(used)(maybeFromAndWarn(warnings)(elem))
      def childElemsAsReverseExtrasExcept( warnings : mutable.Growable[String] )( used : Vector[Elem] )( elem : Elem )( using pconfig : Parser.Config ) : List[Extra] =
        val (ws, elems) = childElemsAsReverseExtrasExcept( used )( elem )
        warnings ++= ws
        elems
      def reverseExtrasExcept( warnings : mutable.Growable[String] )( nonExtra : Vector[Elem] )( nlist : List[Node] )( using pconfig : Parser.Config ) : List[Element.Extra] =
        val ( ws, rextras ) = reverseExtrasExcept( nonExtra )( nlist )
        warnings ++= ws
        rextras
      def allChildElemsAsReverseExtras( warnings : mutable.Growable[String] )( elem : Elem )( using pconfig : Parser.Config ) : List[Element.Extra] =
        val ( ws, rextras ) = allChildElemsAsReverseExtras( elem )
        warnings ++= ws
        rextras

      val defaultPrefix = namespace.fold(null)(_.prefix)
      val kind = Kind( namespace, label )

      def in( kinds : Kinds ) : Boolean = Kinds.contains( kinds, this.kind )

    object ToXml:
        object Spec:
            val Default = Spec(120, 2, true, identity, false)
        case class Spec(
            prettyPrintWidth           : Int,
            prettyPrintStep            : Int,
            prettyPrintMinimizeEmpty   : Boolean,
            postprocessor              : Node => Node,
            guessUnspecifiedNamespaces : Boolean,
        )

trait Element[T <: Element[T]]:
    self : T =>
    import Element.{Extra,ToXml}

    val namespaces      : List[Namespace]
    val reverseExtras   : List[Extra]
    val extraAttributes : MetaData
    val asLastParsed    : Option[Elem]

    def overNamespaces(namespaces : List[Namespace]) : T

    def reverseExtras( newReverseExtras : List[Extra] ) : T

    def withExtra(extra : Extra)        : T = reverseExtras( extra :: reverseExtras )
    def withExtra(elem : Elem)          : T = withExtra( Extra(None,elem) )
    def withExtra(element : Element[?]) : T = withExtra( Extra(Some(element),element.toElem) )


    def withExtras( extras : Iterable[Extra] ) : T =
        reverseExtras( extras.foldLeft( reverseExtras )( ( accum, next ) => next :: accum ) )

    @targetName("withExtraElems")
    def withExtras( elems : Iterable[Elem] ) : T =
        withExtras( elems.map(elem => Extra(None,elem)) )

    @targetName("withExtraElements")
    def withExtras(elements: Iterable[Element[?]]): T =
        withExtras( elements.map(element => Extra(Some(element),element.toElem)) )

    def withNoExtras() : T = reverseExtras(Nil)

    lazy val extras        = reverseExtras.reverse
    lazy val extraElems    = extras.map( _.elem )
    lazy val extraElements = extras.map( _.mbSourceElement ).collect { case Some(element) => element }

    def toUndecoratedElem : Elem

    def reorderedChildren(rawChildren : List[Node]) : List[Node] = rawChildren

    lazy val toElem : Elem =
        val simple = this.toUndecoratedElem
        simple.copy(scope=Namespace.toBinding(this.namespaces), attributes=simple.attributes.append(extraAttributes), child=reorderedChildren(simple.child.toList ::: this.extraElems))

    lazy val guessUnspecifiedNamespacesReferenced : Option[List[Namespace]] =
        if this.namespaces.nonEmpty then
            None // we don't guess if namespaces have been provided
        else
            val seen = mutable.Set.empty[Node]
            val prefixes = mutable.Set.empty[String]

            def check(node : Node) : Unit =
                if !seen(node) then
                    seen += node
                    node match
                        case elem : Elem if elem.prefix != null => prefixes += node.prefix
                        case _ => /* ignore */
                    node.child.foreach( check )

            check( this.toElem )

            // XXX: Should we log unrecognized prefixes?
            Some( prefixes.map(Namespace.byPrefix).collect { case Some( ns : Namespace ) => ns}.toList )

    def asPostprocessed( toXmlSpec : ToXml.Spec ) : Node =
        val elem =
            if toXmlSpec.guessUnspecifiedNamespaces then
                guessUnspecifiedNamespacesReferenced match
                    case Some(list) => this.toElem.copy(scope=Namespace.toBinding(list))
                    case _          => this.toElem
            else
                this.toElem
        toXmlSpec.postprocessor(elem)

    private def asXmlText( postprocessed : Node, toXmlSpec : ToXml.Spec ) : String =
        val pp = new PrettyPrinter(width=toXmlSpec.prettyPrintWidth, step=toXmlSpec.prettyPrintStep, minimizeEmpty=toXmlSpec.prettyPrintMinimizeEmpty )
        val noXmlDeclarationPretty = pp.format(postprocessed)
        s"<?xml version='1.0' encoding='UTF-8'?>\n${noXmlDeclarationPretty}"

    def asXmlText( toXmlSpec : ToXml.Spec ) : String = asXmlText(this.asPostprocessed( toXmlSpec ), toXmlSpec )

    def bytes( toXmlSpec : ToXml.Spec ) : immutable.Seq[Byte] =
        val xmlBytes = asXmlText(toXmlSpec).getBytes(scala.io.Codec.UTF8.charSet)
        immutable.ArraySeq.ofByte(xmlBytes)

    lazy val asPostprocessed : Node = this.asPostprocessed( ToXml.Spec.Default )

    lazy val asXmlText : String = this.asXmlText( asPostprocessed, ToXml.Spec.Default )

    lazy val bytes : immutable.ArraySeq[Byte] = immutable.ArraySeq.ofByte(asXmlText.getBytes(scala.io.Codec.UTF8.charSet))






