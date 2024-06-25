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
          def lenientParse( string : String ) : Option[LinkRelation] = LinkRelation.values.find( _.toString.equalsIgnoreCase( string ) )
        enum LinkRelation:
            case alternate, related, self, enclosure, via

        // for now, probably forever
        // see https://datatracker.ietf.org/doc/html/rfc3987
        // via https://datatracker.ietf.org/doc/html/rfc4287#section-4.2.7.2
        opaque type Iri = String
        def toIri( s : String ) : Iri = s

        object Link extends Parser[Link]("link", Some(Namespace.Atom)):
          def fromChecked( elem : Elem, retainParsed : Boolean ) : ( Seq[String], Option[Link] ) =
            val warnings = Vector.newBuilder[String]
            val reverseExtras = allChildElemsAsReverseExtras(elem, retainParsed)
            val extraAttributes = attributesBeyond("href","rel","type","hreflang","title","length")( elem.attributes )
            val asLastParsed = if retainParsed then Some(elem) else None
            val mbHref = getAttr(elem.attributes)("href")
            val mbRel = getAttr(elem.attributes)("rel").map( raw => LinkRelation.lenientParse(raw).getOrElse( toIri(raw) ) )
            val mbType = getAttr(elem.attributes)("type")
            val mbHrefLang =
              getAttr(elem.attributes)("hreflang").flatMap: raw =>
                LanguageCode.byRendered.get(raw).orElse:
                  warnings += "Could not parse atom:link hreflang '${raw}' to a known language code. Omitting!"
                  None
            val mbTitle = getAttr(elem.attributes)("title")
            val mbLength =
              getAttr(elem.attributes)("length").flatMap: raw =>
                try Some(raw.toLong)
                catch
                  case NonFatal(t) =>
                    warnings += s"Failed to parse atom:link length '${raw}' to a valid Long. Omitting! (${t})"
                    None
            mbHref match
              case Some(href) =>
                ( warnings.result, Some( Link( href, mbRel, mbType, mbHrefLang, mbTitle, mbLength, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) )
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

        object Summary extends Parser[Summary]("summary", Some(Namespace.Atom)):
          def fromChecked( elem : Elem, retainParsed : Boolean ) : ( Seq[String], Option[Summary] ) =
            val reverseExtras = allChildElemsAsReverseExtras(elem, retainParsed)
            val extraAttributes = elem.attributes
            val asLastParsed = if retainParsed then Some(elem) else None
            ( Nil, Some( Summary( elem.text , reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) )
        case class Summary( text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None ) extends Element[Summary]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = new Elem(prefix="atom", label="summary", attributes1=Null, scope=TopScope, minimizeEmpty=true, new PCData(text))

        object Updated extends Parser[Updated]("updated", Some(Namespace.Atom)):
            def fromChecked( elem : Elem, retainParsed : Boolean ) : ( Seq[String], Option[Updated] ) =
              try
                val reverseExtras = allChildElemsAsReverseExtras(elem, retainParsed)
                val extraAttributes = elem.attributes
                val asLastParsed = if retainParsed then Some(elem) else None
                ( Nil, Some( Updated(ZonedDateTime.parse( elem.text ), reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) )
              catch
                case NonFatal(t) => ( Seq( s"Could not parse text '${elem.text}' as timestamp: $t"), None )
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
          def lenientParse( string : String ) : Option[Value] = Value.values.find( _.toString.equalsIgnoreCase( string ) )
        enum Value:
          case Ping, Metadata, Content, Media
      case class Completeness( value : Completeness.Value, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Completeness]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
          Elem(prefix = "iffy", label = "completeness", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(value.toString))
      case class Diff( url : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Diff]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
          Elem(prefix = "iffy", label = "diff", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(url))
      object HintAnnounce extends Parser[HintAnnounce]("hint-announce",Some(Namespace.Iffy)):
        object Policy:
          def lenientParse( string : String ) : Option[Policy] = Policy.values.find( _.toString.equalsIgnoreCase( string ) )
        enum Policy:
          case Always, Never, Piggyback
        // XXX: It'd be better to check all the conditions and warn them all, rather
        //      than failing of first error
        def fromChecked( elem : Elem, retainParsed : Boolean ) : ( Seq[String], Option[HintAnnounce] ) =
          val warnings = Vector.newBuilder[String]
          val extraReverseExtras = childElemsBeyondAsReverseExtras( (Some(Namespace.Iffy),"policy")->1, (Some(Namespace.Iffy),"restriction") )(elem, retainParsed)
          val extraAttributes = elem.attributes
          val asLastParsed = if retainParsed then Some(elem) else None
          val policyElems =
            val (w, e) = Iffy.Policy.extractFromChildren( elem, retainParsed )
            warnings ++= w
            e
          if policyElems.isEmpty then
            warnings += s"iffy:hint-announce expects exactly an iffy:policy sub-element, found none, skipping."
            (warnings.result, None)
          else
            policyElems.foreach: pe =>
              if Policy.lenientParse( pe.value ).isEmpty then
                warnings += s"Encountered unknown hint-announce policy value: '${pe.value}'"
            val restrictionElems =
              val (w, e) = Iffy.Restriction.extractFromChildren( elem, retainParsed )
              warnings ++= w
              e
            val mbFirstRestriction = restrictionElems.headOption
            val restrictionReverseExtras = if restrictionElems.nonEmpty then restrictionElems.tail.reverse.map( Extra.apply ).toList else Nil
            val policyReverseExtras = policyElems.tail.reverse.map( Extra.apply ).toList
            (warnings.result, Some(HintAnnounce(policyElems.head,mbFirstRestriction, reverseExtras = restrictionReverseExtras ::: policyReverseExtras ::: extraReverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed)))
        end fromChecked
        def apply( policy : Iffy.HintAnnounce.Policy, restriction : Option[Restriction] ) : HintAnnounce = apply( Iffy.Policy(policy.toString), restriction )
        def apply( policy : Iffy.HintAnnounce.Policy ) : HintAnnounce = apply( Iffy.Policy(policy.toString) )
      end HintAnnounce 
      case class HintAnnounce( policy : Iffy.Policy, restriction : Option[Restriction] = None, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[HintAnnounce]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            Elem(prefix = "iffy", label = "hint-announce", attributes = Null, scope = TopScope, minimizeEmpty = true, child = (Seq(policy.toElem) ++ restriction.map(_.toElem))*)
      case class Initial(creators : Seq[DublinCore.Creator], namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None ) extends Element[Initial]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem(prefix="iffy")(label="initial", creators.map(_.toElem)*)
      case class OriginalGuid( value : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[OriginalGuid]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
          Elem(prefix = "iffy", label = "original-guid", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(value))
      object Policy extends Parser[Policy]("policy",Some(Namespace.Iffy)):
        def fromChecked( elem : Elem, retainParsed : Boolean ) : ( Seq[String], Option[Policy] ) =
          val reverseExtras = allChildElemsAsReverseExtras(elem, retainParsed)
          val extraAttributes = elem.attributes
          val asLastParsed = if retainParsed then Some(elem) else None
          ( Nil, Some( Policy( elem.text.trim, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) )
      case class Policy( value : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Policy]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            Elem(prefix = "iffy", label = "policy", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(value))
      object Provenance:
        enum Shape:
          case sequence, merge
      case class Provenance( links : Seq[Atom.Link], childProvenances : Seq[Provenance],  shape : Option[Provenance.Shape] = None, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Provenance]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val attributes = shape.fold(Null)( shape => new UnprefixedAttribute("shape", shape.toString, Null) )
            Elem(prefix = "iffy", label = "provenance", attributes = attributes, scope = TopScope, minimizeEmpty = true, child = (links.map(_.toElem) ++ childProvenances.map(_.toElem))*)
      object Restriction extends Parser[Restriction]("restriction",Some(Namespace.Iffy)):
        def fromChecked( elem : Elem, retainParsed : Boolean ) : ( Seq[String], Option[Restriction] ) =
          val reverseExtras = allChildElemsAsReverseExtras(elem, retainParsed)
          val extraAttributes = elem.attributes
          val asLastParsed = if retainParsed then Some(elem) else None
          ( Nil, Some( Restriction(elem.child, reverseExtras = reverseExtras, extraAttributes = extraAttributes, asLastParsed = asLastParsed) ) )
      case class Restriction( child : Seq[Node] = Nil, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Restriction]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            Elem(prefix = "iffy", label = "restriction", attributes = Null, scope = TopScope, minimizeEmpty = true, child = child*)
      case class Revision( url : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Revision]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
          Elem(prefix = "iffy", label = "revision", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(url))
      object Synthetic:
        object KnownType:
          val ItemUpdateFeed     = "ItemUpdateFeed"
          val UpdateAnnouncement = "UpdateAnnouncement"
          val UpdateCumulation   = "UpdateCumulation"
      case class Synthetic( `type` : Option[Type] = None, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Synthetic]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            Elem(prefix = "iffy", label = "synthetic", attributes = Null, scope = TopScope, minimizeEmpty = true, child = `type`.map(_.toElem).toSeq* )
      case class Type( value : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[Type]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            Elem(prefix = "iffy", label = "type", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new Text(value))
      case class Update(
        updated : Atom.Updated,
        summary : Option[Atom.Summary],
        revision : Option[Revision],
        diff : Option[Diff],
        creators : Seq[DublinCore.Creator],
        namespaces : List[Namespace] = Nil,
        reverseExtras : List[Extra] = Nil,
        extraAttributes : MetaData = Null,
        asLastParsed : Option[Elem] = None
      ) extends Element[Update]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem(prefix="iffy")(label="update", (Seq(updated)++summary++revision++diff++creators).map(_.toElem)*)
      case class UpdateHistory( updates : Seq[Update], initial : Option[Initial], namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil, extraAttributes : MetaData = Null, asLastParsed : Option[Elem] = None) extends Element[UpdateHistory]:
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

    /**
     *  A helper, not itself an element.
     */
    object Extra:
      def apply( sourceElement : Element[?] ) : Extra = Extra( Some(sourceElement), sourceElement.toElem )
      def apply( unpairedElem  : Elem       ) : Extra = Extra( None, unpairedElem )
    case class Extra( mbSourceElement : Option[Element[?]], elem : Elem )

    trait Parser[T <: Element[T]]( val label : String, val namespace : Option[Namespace]) extends ParserUtils:
      def fromChecked( elem : Elem, retainParsed : Boolean ) : ( Seq[String], Option[T] ) 
      def check( elem : Elem ) : Boolean =
        elem.label == label && namespace.fold( defaultNamespaceUri(elem.scope) == None )( ns => ns.belongsLenient(elem) )
      def maybeFrom( elem : Elem, retainParsed : Boolean ) : ( Seq[String], Option[T] ) =
        if check( elem ) then fromChecked( elem, retainParsed ) else ( Nil, None )
      def extractFromChildren( parent : Elem, retainParsed : Boolean ) : (Seq[String], Seq[T]) =
        parent.child.foldLeft( (Vector.empty[String], Vector.empty[T] ) ): ( accum, next ) =>
          next match
            case elem : Elem  =>
              val (newWarnings, mbParsed)      = maybeFrom( elem, retainParsed )
              val (oldWarnings, alreadyParsed) = accum // XXX: we extract rather than use accum(0), accum(1) 'cuz weird compiler errors, Scala 3.3.3
              ( oldWarnings ++ newWarnings, alreadyParsed ++ mbParsed )
            case other =>
              accum
      val prefix = namespace.fold(null)(_.prefix)

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






