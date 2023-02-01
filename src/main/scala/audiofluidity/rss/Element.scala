package audiofluidity.rss

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME
import scala.collection.*
import scala.xml.{Elem, MetaData, Node, Null, PCData, PrettyPrinter, Text, TopScope, UnprefixedAttribute}

import scala.annotation.targetName

object Element:
    val RssVersion = "2.0"

    private val RssDateTimeFormatter = RFC_1123_DATE_TIME

    private val DefaultPrettyPrintSpec = PrettyPrintSpec( width = 120, step = 2, minimizeEmpty = true )

    private def elem(label : String, attributes1 : MetaData, children : Node*) : Elem =
        new Elem(prefix=null, label=label, attributes1=attributes1, scope=TopScope, minimizeEmpty=true, children : _*)

    private def elem(label : String, children : Node*) : Elem = elem(label, Null, children : _*)

    enum ValidDay:
        case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

    case class Author(email : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Author]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("author", new Text(this.email))

    case class Category(domain : String, text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Category]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            elem("category", new UnprefixedAttribute("domain", this.domain, Null), new Text(this.text))

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
          def create( spec : Spec, items : immutable.Seq[Item]) : Channel =
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
          def create (
                title              : String,
                linkUrl            : String,
                description        : String,
                items              : immutable.Seq[Item],
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
                      items = items
                )
    case class Channel(
        title          : Title,
        link           : Link,
        description    : Description,
        language       : Option[Language] = None,
        copyright      : Option[Copyright] = None,
        managingEditor : Option[ManagingEditor] = None,
        webMaster      : Option[WebMaster] = None,
        pubDate        : Option[PubDate] = None,
        lastBuildDate  : Option[LastBuildDate] = None,
        categories     : immutable.Seq[Category] = immutable.Seq.empty,
        generator      : Option[Generator] = None,
        docs           : Option[Docs] = Some(Docs()), // use default docs URL
        cloud          : Option[Cloud] = None,
        ttl            : Option[Ttl] = None,
        image          : Option[Image] = None,
        rating         : Option[Rating] = None,
        textInput      : Option[TextInput] = None,
        skipHours      : Option[SkipHours] = None,
        skipDays       : Option[SkipDays] = None,
        items          : immutable.Seq[Item],
        namespaces     : List[Namespace] = Nil,
        reverseExtras  : List[Extra] = Nil,
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

    case class Cloud(domain : String, port : Int, path : String, registerProcedure : String, protocol : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Cloud]:
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

    case class Comments(url : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Comments]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("comments", new Text(this.url))

    case class Copyright(notice : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Copyright]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("copyright", new Text(this.notice))

    case class Day(day : ValidDay, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Day]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("day", new Text(this.day.toString))

    case class Description(text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Description]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("description", new PCData(this.text))

    case class Docs(url : String = "https://cyber.harvard.edu/rss/rss.html", namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Docs]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("docs", new Text(this.url))

    case class Enclosure(url : String, length : Long, `type` : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Enclosure]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val attributes = UnprefixedAttribute("url", this.url,
                new UnprefixedAttribute("length", this.length.toString,
                    new UnprefixedAttribute("type", this.`type`, Null)
                )
            )
            elem("enclosure", attributes )

    case class Generator(description : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Generator]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("generator", new Text(this.description))

    case class Guid(isPermalink : Boolean, id : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Guid]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("guid", new UnprefixedAttribute("isPermalink", this.isPermalink.toString, Null), new Text(this.id))

    // should be 1 to 24
    case class Hour(hour : Int, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Hour]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("hour", new Text(this.hour.toString))

    case class Height(pixels : Int, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Height]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("height", new Text(this.pixels.toString))

    case class Image(url : Url, title : Title, link : Link, width : Option[Width], height : Option[Height], description : Option[Description], namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Image]:
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
            categories  : immutable.Seq[Category],
            comments    : Option[String],
            enclosure   : Option[Enclosure],
            guid        : Option[Guid],
            pubDate     : Option[ZonedDateTime],
            source      : Option[Source],
        ) : Item = this.apply (
            Title(title),
            Link(linkUrl),
            Description(description),
            Author(author),
            categories,
            comments.map(Comments(_)),
            enclosure,
            guid,
            pubDate.map(PubDate(_)),
            source
        )
    case class Item(
        title         : Title,
        link          : Link,
        description   : Description,
        author        : Author,
        categories    : immutable.Seq[Category] = immutable.Seq.empty,
        comments      : Option[Comments]        = None,
        enclosure     : Option[Enclosure]       = None,
        guid          : Option[Guid]            = None,
        pubDate       : Option[PubDate]         = None,
        source        : Option[Source]          = None,
        namespaces    : List[Namespace]         = Nil,
        reverseExtras : List[Extra]              = Nil,
    ) extends Element[Item]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val kids =
                this.categories.map(_.toElem) ++ this.source.map(_.toElem) ++ this.pubDate.map(_.toElem) ++ this.guid.map(_.toElem) ++ this.enclosure.map(_.toElem) ++
                  this.comments.map(_.toElem) :+ this.author.toElem :+ this.description.toElem :+ this.link.toElem :+ this.title.toElem
            elem("item", kids : _*)

    case class Language(code : LanguageCode, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Language]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("language", new Text(this.code.rendered))

    case class LastBuildDate(date : ZonedDateTime, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[LastBuildDate]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val dateStr = this.date.format(RssDateTimeFormatter)
            elem("lastBuildDate", new Text(dateStr))

    case class Link(location : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Link]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("link", new Text(this.location))

    case class ManagingEditor(email : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[ManagingEditor]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("managingEditor", new Text(this.email))

    case class Name(text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Name]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("name", new Text(this.text))

    case class PubDate(date : ZonedDateTime, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[PubDate]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem: Elem =
            val dateStr = this.date.format(RssDateTimeFormatter)
            elem("pubDate", new Text(dateStr))

    // this seems widely unutilized, not sure what the contents might look like exactly
    case class Rating(contents : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Rating]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("rating", new Text(this.contents))

    case class Rss( channel : Channel, version : String = RssVersion, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Rss]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem =
            elem("rss", new UnprefixedAttribute("version", RssVersion, Null), this.channel.toElem)

    case class SkipDays(days : immutable.Seq[Day], namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[SkipDays]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("skipDays", this.days.map( _.toElem ) : _*)

    case class SkipHours(hours : immutable.Seq[Hour], namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[SkipHours]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("skipHours", this.hours.map( _.toElem ) : _*)

    case class Source(url : String, title : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Source]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("source", new UnprefixedAttribute("url",this.url,Null), new Text(this.title))

    // mostly unutilized
    case class TextInput(title : Title, description : Description, name : Name, link : Link, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[TextInput]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("textInput", this.title.toElem, this.description.toElem, this.name.toElem, this.link.toElem)

    case class Title(text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Title]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("title", new Text(this.text))

    case class Ttl(minutes : Int, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Ttl]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("ttl", new Text(this.minutes.toString))

    case class Url(location : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Url]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("url", new Text(this.location))

    case class WebMaster(email : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[WebMaster]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("webMaster", new Text(this.email))

    case class Width(pixels : Int, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Width]:
        override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
        override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
        override def toUndecoratedElem : Elem = elem("width", new Text(this.pixels.toString))

    object Atom:
        enum LinkRelation:
            case alternate, related, self, enclosure, via

        // for now, probably forever
        // see https://datatracker.ietf.org/doc/html/rfc3987
        // via https://datatracker.ietf.org/doc/html/rfc4287#section-4.2.7.2
        opaque type Iri = String
        def toIri( s : String ) : Iri = s

        case class Link(
          href     : String,
          rel      : Option[LinkRelation | Iri] = None,
          `type`   : Option[String]             = None,
          hreflang : Option[LanguageCode]       = None,
          title    : Option[String]             = None,
          length   : Option[Long]               = None,
          namespaces : List[Namespace]          = Nil,
          reverseExtras : List[Extra]            = Nil,
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


    object Content:
        case class Encoded(text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Encoded]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = new Elem(prefix="content", label="encoded", attributes1=Null, scope=TopScope, minimizeEmpty=true, new PCData(this.text))


    object DublinCore:
        case class Creator(creator : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Creator]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem: Elem =
                Elem(prefix = "dc", label = "creator", attributes = Null, scope = TopScope, minimizeEmpty = true, child = new PCData(this.creator))

    // Apple-specific elements
    private def ielem(label : String, attributes1 : MetaData, children : Node*) : Elem =
        new Elem(prefix="itunes", label=label, attributes1=attributes1, scope=TopScope, minimizeEmpty=true, children : _*)
    private def ielem(label : String, children : Node*) : Elem = ielem(label, Null, children : _*)

    object Itunes:
        enum ValidPodcastType:
            case episodic, serial
        enum ValidEpisodeType:
            case full, trailer, bonus

        case class Author(fullName : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Author]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("author", new Text(this.fullName))

        // should always contain "Yes"
        case class Block(namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Block]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("block", new Text("Yes"))

        case class Category(text : String, subcategory : Option[Itunes.Category] = None, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Category]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem: Elem =
                ielem("category", new UnprefixedAttribute("text", this.text, Null), this.subcategory.map(_.toElem).toSeq : _*)

        // should always contain "Yes"
        case class Complete(namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Complete]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("complete", new Text("Yes"))

        case class Duration(seconds : Long, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Duration]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("duration", new Text(this.seconds.toString))

        case class Email(email : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Email]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("email", new Text(this.email))

        case class Episode(number : Int, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Episode]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("episode", new Text(this.number.toString))

        case class EpisodeType(validEpisodeType : Itunes.ValidEpisodeType, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[EpisodeType]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("episodeType", new Text(this.validEpisodeType.toString))

        case class Explicit(isExplicit : Boolean, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Explicit]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("explicit", new Text(this.isExplicit.toString))

        case class Image(href : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Image]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem: Elem = ielem("image", new UnprefixedAttribute("href", this.href, Null) )

        case class Keywords(keywords : immutable.Seq[String], namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Keywords]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("keywords", new Text(this.keywords.mkString(",")))

        case class Name(name : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Name]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("name", new Text(this.name))

        case class NewFeedUrl(location : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[NewFeedUrl]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("new-feed-url", new Text(this.location))

        case class Owner(name : Itunes.Name, email : Itunes.Email, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Owner]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("owner", this.name.toElem, this.email.toElem)

        case class Season(number : Int, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Season]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("season", new Text(this.number.toString))

        case class Subtitle(text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Subtitle]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("subtitle", new Text(this.text))

        case class Summary(text : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Summary]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("summary", new Text(this.text))

        case class Title(title : String, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Title]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("title", new Text(this.title))

        case class Type(validType : Itunes.ValidPodcastType, namespaces : List[Namespace] = Nil, reverseExtras : List[Extra] = Nil) extends Element[Type]:
            override def overNamespaces(namespaces : List[Namespace]) = this.copy(namespaces = namespaces)
            override def reverseExtras( newReverseExtras : List[Extra] ) = this.copy( reverseExtras = newReverseExtras )
            override def toUndecoratedElem : Elem = ielem("type", new Text(this.validType.toString))

    /**
     *  A helper, not itself an element.
     */
    case class Extra( mbSourceElement : Option[Element[?]], elem : Elem )

    /**
     * A helper, not itself an element.
     */
    case class PrettyPrintSpec( width : Int, step : Int, minimizeEmpty : Boolean )

trait Element[T <: Element[T]]:
    self : T =>
    import Element.{Extra,PrettyPrintSpec}

    val namespaces    : List[Namespace]
    val reverseExtras : List[Extra]

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

    def toElem : Elem =
        val simple = this.toUndecoratedElem
        simple.copy(scope=Namespace.toBinding(this.namespaces), child=(simple.child.toList ::: this.extraElems))

    def asXmlText(pps : PrettyPrintSpec = Element.DefaultPrettyPrintSpec, transformer : Node => Node = identity ) : String =
        val pp = new PrettyPrinter(width=pps.width, step=pps.step, minimizeEmpty=pps.minimizeEmpty )
        val noXmlDeclarationPretty = pp.format(transformer(this.toElem))
        s"<?xml version='1.0' encoding='UTF-8'?>\n${noXmlDeclarationPretty}"

    def bytes( pps : PrettyPrintSpec = Element.DefaultPrettyPrintSpec, transformer : Node => Node = identity ) : immutable.Seq[Byte] =
        immutable.ArraySeq.ofByte(asXmlText(pps,transformer).getBytes(scala.io.Codec.UTF8.charSet))

    lazy val asXmlText : String = asXmlText( Element.DefaultPrettyPrintSpec, identity )

    lazy val bytes : immutable.Seq[Byte] = immutable.ArraySeq.ofByte(asXmlText.getBytes(scala.io.Codec.UTF8.charSet))






