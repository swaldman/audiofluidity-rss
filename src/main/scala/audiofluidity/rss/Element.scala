package audiofluidity.rss

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME

import scala.collection.*
import scala.xml.{Elem, MetaData, Node, Null, PCData, PrettyPrinter, Text, TopScope, UnprefixedAttribute}


object Element:
    val RssVersion = "2.0"

    private val RssDateTimeFormatter = RFC_1123_DATE_TIME

    private def elem(label : String, attributes1 : MetaData, children : Node*) : Elem =
        new Elem(prefix=null, label=label, attributes1=attributes1, scope=TopScope, minimizeEmpty=true, children : _*)

    private def elem(label : String, children : Node*) : Elem = elem(label, Null, children : _*)

    enum ValidDay:
        case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

    case class Author(email : String, reverseExtras : List[Elem] = Nil) extends Element[Author]:
        override def withExtra( elem : Elem ) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("author", cxtras(new Text(this.email)) :_*)

    case class Category(domain : String, text : String, reverseExtras : List[Elem] = Nil) extends Element[Category]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem: Elem =
            elem("category", new UnprefixedAttribute("domain", this.domain, Null), cxtras(new Text(this.text)) :_*)

    object Channel:
        val dummy = 1
//          def create (
//                title              : String,
//                linkUrl            : String,
//                description        : String,
//                items              : immutable.Seq[Item],
//                language           : Option[LanguageCode]            = None,
//                copyright          : Option[String]                  = None,
//                managingEditor     : Option[String]                  = None,
//                webmaster          : Option[String]                  = None,
//                pubDate            : Option[ZonedDateTime]           = None,
//                lastBuildDate      : Option[ZonedDateTime]           = None,
//                categories         : immutable.Seq[Element.Category] = Nil,
//                generator          : Option[String]                  = None,
//                cloud              : Option[Element.Cloud]           = None,
//                ttlMinutes         : Option[Int]                     = None,
//                image              : Option[Element.Image]           = None,
//                rating             : Option[String]                  = None,
//                textInput          : Option[Element.TextInput]       = None,
//                skipHours          : Option[Element.SkipHours]       = None,
//                skipDays           : Option[Element.SkipDays]        = None,
//                channelDecorations : immutable.Seq[Elem]             = Nil,
//                rssElemPostProcess : Elem => Elem                    = identity,
//                namespaces         : List[Namespace]                 = Nil
//          ) : RssFeed =
//
//            // where we had convenience circumventions, we have to build our elements
//            val elemTitle = Element.Title(title)
//            val elemLink  = Element.Link(linkUrl)
//            val elemDesc  = Element.Description(description)
//
//            val mbLanguage       = language.map( Element.Language.apply )
//            val mbCopyright      = copyright.map( Element.Copyright.apply )
//            val mbManagingEditor = managingEditor.map( Element.ManagingEditor.apply )
//            val mbWebmaster      = webmaster.map( Element.WebMaster.apply )
//            val mbPubDate        = pubDate.map( Element.PubDate.apply )
//            val mbLastBuildDate  = lastBuildDate.map( Element.LastBuildDate.apply )
//            val mbGenerator      = generator.map( Element.Generator.apply )
//            val mbTtl            = ttlMinutes.map( Element.Ttl.apply )
//            val mbRating         = rating.map( Element.Rating.apply )
//
//            val channel = Element.Channel(
//              title = elemTitle,
//              link = elemLink,
//              description = elemDesc,
//              language = mbLanguage,
//              copyright = mbCopyright,
//              managingEditor = mbManagingEditor,
//              webMaster = mbWebmaster,
//              pubDate = mbPubDate,
//              lastBuildDate = mbLastBuildDate,
//              categories = categories,
//              generator = mbGenerator,
//              docs = Some(Element.Docs()), // use the default docs URL pretty much always
//              cloud = cloud,
//              ttl = mbTtl,
//              image = image,
//              rating = mbRating,
//              textInput = textInput,
//              skipHours = skipHours,
//              skipDays = skipDays,
//              items = items
//            )

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
        reverseExtras : List[Elem] = Nil,
    ) extends Element[Channel]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem: Elem =
            val itemElems = this.items.map(_.toElem)
            val kids =
                itemElems ++ this.skipDays.map(_.toElem) ++ this.skipHours.map(_.toElem) ++ this.textInput.map(_.toElem) ++
                  this.rating.map(_.toElem) ++ this.image.map(_.toElem) ++ this.ttl.map(_.toElem) ++ this.cloud.map(_.toElem) ++ this.docs.map(_.toElem) ++
                  this.generator.map(_.toElem) ++ this.categories.map(_.toElem) ++ this.lastBuildDate.map(_.toElem) ++ this.pubDate.map(_.toElem) ++
                  this.webMaster.map(_.toElem) ++ this.managingEditor.map(_.toElem) ++ this.copyright.map(_.toElem) ++ this.language.map(_.toElem) :+
                  this.description.toElem :+ this.link.toElem :+ this.title.toElem
            elem("channel", cxtras(kids.toSeq) : _*)

    case class Cloud(domain : String, port : Int, path : String, registerProcedure : String, protocol : String, reverseExtras : List[Elem] = Nil) extends Element[Cloud]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem: Elem =
            val attributes = new UnprefixedAttribute("domain", this.domain,
                new UnprefixedAttribute("port", this.port.toString,
                    new UnprefixedAttribute("path", this.path,
                        new UnprefixedAttribute("registerProcedure", this.registerProcedure,
                            new UnprefixedAttribute("protocol", this.protocol, Null)
                        )
                    )
                )
            )
            elem("cloud", attributes, cxtras() :_*)

    case class Comments(url : String, reverseExtras : List[Elem] = Nil) extends Element[Comments]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("comments", cxtras(new Text(this.url)) : _*)

    case class Copyright(notice : String, reverseExtras : List[Elem] = Nil) extends Element[Copyright]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("copyright", cxtras(new Text(this.notice)): _*)

    case class Day(day : ValidDay, reverseExtras : List[Elem] = Nil) extends Element[Day]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("day", cxtras(new Text(this.day.toString)):_*)

    case class Description(text : String, reverseExtras : List[Elem] = Nil) extends Element[Description]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("description", cxtras(new PCData(this.text)):_*)

    case class Docs(url : String = "https://cyber.harvard.edu/rss/rss.html", reverseExtras : List[Elem] = Nil) extends Element[Docs]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("docs", cxtras(new Text(this.url)):_*)

    case class Enclosure(url : String, length : Long, `type` : String, reverseExtras : List[Elem] = Nil) extends Element[Enclosure]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem: Elem =
            val attributes = UnprefixedAttribute("url", this.url,
                new UnprefixedAttribute("length", this.length.toString,
                    new UnprefixedAttribute("type", this.`type`, Null)
                )
            )
            elem("enclosure", attributes, cxtras() :_*)

    case class Generator(description : String, reverseExtras : List[Elem] = Nil) extends Element[Generator]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("generator", cxtras(new Text(this.description)):_*)

    case class Guid(isPermalink : Boolean, id : String, reverseExtras : List[Elem] = Nil) extends Element[Guid]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("guid", new UnprefixedAttribute("isPermalink", this.isPermalink.toString, Null), cxtras(new Text(this.id)):_*)

    // should be 1 to 24
    case class Hour(hour : Int, reverseExtras : List[Elem] = Nil) extends Element[Hour]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("hour", cxtras(new Text(this.hour.toString)):_*)

    case class Height(pixels : Int, reverseExtras : List[Elem] = Nil) extends Element[Height]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("height", cxtras(new Text(this.pixels.toString)):_*)

    case class Image(url : Url, title : Title, link : Link, width : Option[Width], height : Option[Height], description : Option[Description], reverseExtras : List[Elem] = Nil) extends Element[Image]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem: Elem =
            val reverseKids: List[Elem] =
                this.description.map(_.toElem) ++: this.height.map(_.toElem) ++: this.width.map(_.toElem) ++: (this.link.toElem :: this.title.toElem :: this.url.toElem :: Nil)
            elem("image", cxtras(): _*)

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
        reverseExtras : List[Elem]              = Nil,
    ) extends Element[Item]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem: Elem =
            val kids =
                this.categories.map(_.toElem) ++ this.source.map(_.toElem) ++ this.pubDate.map(_.toElem) ++ this.guid.map(_.toElem) ++ this.enclosure.map(_.toElem) ++
                  this.comments.map(_.toElem) :+ this.author.toElem :+ this.description.toElem :+ this.link.toElem :+ this.title.toElem
            elem("item", cxtras(kids): _*)

    case class Language(code : LanguageCode, reverseExtras : List[Elem] = Nil) extends Element[Language]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("language", cxtras(new Text(this.code.rendered)):_*)

    case class LastBuildDate(date : ZonedDateTime, reverseExtras : List[Elem] = Nil) extends Element[LastBuildDate]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem: Elem =
            val dateStr = this.date.format(RssDateTimeFormatter)
            elem("lastBuildDate", cxtras(new Text(dateStr)):_*)

    case class Link(location : String, reverseExtras : List[Elem] = Nil) extends Element[Link]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("link", cxtras(new Text(this.location)):_*)

    case class ManagingEditor(email : String, reverseExtras : List[Elem] = Nil) extends Element[ManagingEditor]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("managingEditor", cxtras(new Text(this.email)):_*)

    case class Name(text : String, reverseExtras : List[Elem] = Nil) extends Element[Name]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("name", cxtras(new Text(this.text)):_*)

    case class PubDate(date : ZonedDateTime, reverseExtras : List[Elem] = Nil) extends Element[PubDate]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem: Elem =
            val dateStr = this.date.format(RssDateTimeFormatter)
            elem("pubDate", cxtras(new Text(dateStr)):_*)

    // this seems widely unutilized, not sure what the contents might look like exactly
    case class Rating(contents : String, reverseExtras : List[Elem] = Nil) extends Element[Rating]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("rating", cxtras(new Text(this.contents)):_*)

    case class Rss( version : String = RssVersion, channel : Channel, reverseExtras : List[Elem] = Nil) extends Element[Rss]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem =
            elem("rss", new UnprefixedAttribute("version", RssVersion, Null), cxtras(this.channel.toElem):_*)

    case class SkipDays(days : immutable.Seq[Day], reverseExtras : List[Elem] = Nil) extends Element[SkipDays]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("skipDays", cxtras(this.days.map( _.toElem )) : _*)

    case class SkipHours(hours : immutable.Seq[Hour], reverseExtras : List[Elem] = Nil) extends Element[SkipHours]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("skipHours", cxtras(this.hours.map( _.toElem )) : _*)

    case class Source(url : String, title : String, reverseExtras : List[Elem] = Nil) extends Element[Source]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("source", new UnprefixedAttribute("url",this.url,Null), cxtras(new Text(this.title)):_*)

    // mostly unutilized
    case class TextInput(title : Title, description : Description, name : Name, link : Link, reverseExtras : List[Elem] = Nil) extends Element[TextInput]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("textInput", cxtras(List(this.title.toElem, this.description.toElem, this.name.toElem, this.link.toElem)):_*)

    case class Title(text : String, reverseExtras : List[Elem] = Nil) extends Element[Title]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("title", cxtras(new Text(this.text)):_*)

    case class Ttl(minutes : Int, reverseExtras : List[Elem] = Nil) extends Element[Ttl]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("ttl", cxtras(new Text(this.minutes.toString)):_*)

    case class Url(location : String, reverseExtras : List[Elem] = Nil) extends Element[Url]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("url", cxtras(new Text(this.location)):_*)

    case class WebMaster(email : String, reverseExtras : List[Elem] = Nil) extends Element[WebMaster]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("webMaster", cxtras(new Text(this.email)):_*)

    case class Width(pixels : Int, reverseExtras : List[Elem] = Nil) extends Element[Width]:
        override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
        override def toElem : Elem = elem("width", cxtras(new Text(this.pixels.toString)):_*)

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
          rel      : Option[LinkRelation | Iri],
          `type`   : Option[String],
          hreflang : Option[LanguageCode],
          title    : Option[String],
          length   : Option[Long],
          reverseExtras : List[Elem] = Nil
        ) extends Element[Link]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem: Elem =
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
                Elem(prefix = "atom", label = "link", attributes = attributes, scope = TopScope, minimizeEmpty = true, cxtras():_*)


    object Content:
        case class Encoded(text : String, reverseExtras : List[Elem] = Nil) extends Element[Encoded]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = new Elem(prefix="content", label="encoded", attributes1=Null, scope=TopScope, minimizeEmpty=true, cxtras(new PCData(this.text)):_*)


    object DublinCore:
        case class Creator(creator : String, reverseExtras : List[Elem] = Nil) extends Element[Creator]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem: Elem =
                Elem(prefix = "dc", label = "creator", attributes = Null, scope = TopScope, minimizeEmpty = true, child = cxtras(new PCData(this.creator)):_*)

    // Apple-specific elements
    private def ielem(label : String, attributes1 : MetaData, children : Node*) : Elem =
        new Elem(prefix="itunes", label=label, attributes1=attributes1, scope=TopScope, minimizeEmpty=true, children : _*)
    private def ielem(label : String, children : Node*) : Elem = ielem(label, Null, children : _*)

    object Itunes:
        enum ValidPodcastType:
            case episodic, serial
        enum ValidEpisodeType:
            case full, trailer, bonus

        case class Author(fullName : String, reverseExtras : List[Elem] = Nil) extends Element[Author]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("author", cxtras(new Text(this.fullName)):_*)

        // should always contain "Yes"
        case class Block(reverseExtras : List[Elem] = Nil) extends Element[Block]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("block", cxtras(new Text("Yes")):_*)

        case class Category(text : String, subcategory : Option[Itunes.Category] = None, reverseExtras : List[Elem] = Nil) extends Element[Category]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem: Elem =
                ielem("category", new UnprefixedAttribute("text", this.text, Null), cxtras(this.subcategory.map(_.toElem).toSeq): _*)

        // should always contain "Yes"
        case class Complete(reverseExtras : List[Elem] = Nil) extends Element[Complete]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("complete", cxtras(new Text("Yes")):_*)

        case class Duration(seconds : Long, reverseExtras : List[Elem] = Nil) extends Element[Duration]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("duration", cxtras(new Text(this.seconds.toString)):_*)

        case class Email(email : String, reverseExtras : List[Elem] = Nil) extends Element[Email]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("email", cxtras(new Text(this.email)):_*)

        case class Episode(number : Int, reverseExtras : List[Elem] = Nil) extends Element[Episode]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("episode", cxtras(new Text(this.number.toString)):_*)

        case class EpisodeType(validEpisodeType : Itunes.ValidEpisodeType, reverseExtras : List[Elem] = Nil) extends Element[EpisodeType]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("episodeType", cxtras(new Text(this.validEpisodeType.toString)):_*)

        case class Explicit(isExplicit : Boolean, reverseExtras : List[Elem] = Nil) extends Element[Explicit]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("explicit", cxtras(new Text(this.isExplicit.toString)):_*)

        case class Image(href : String, reverseExtras : List[Elem] = Nil) extends Element[Image]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem: Elem = ielem("image", new UnprefixedAttribute("href", this.href, Null), cxtras() :_*)

        case class Keywords(keywords : immutable.Seq[String], reverseExtras : List[Elem] = Nil) extends Element[Keywords]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("keywords", cxtras(new Text(this.keywords.mkString(","))):_*)

        case class Name(name : String, reverseExtras : List[Elem] = Nil) extends Element[Name]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("name", cxtras(new Text(this.name)):_*)

        case class NewFeedUrl(location : String, reverseExtras : List[Elem] = Nil) extends Element[NewFeedUrl]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("new-feed-url", cxtras(new Text(this.location)):_*)

        case class Owner(name : Itunes.Name, email : Itunes.Email, reverseExtras : List[Elem] = Nil) extends Element[Owner]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("owner", cxtras(List(this.name.toElem, this.email.toElem)):_*)

        case class Season(number : Int, reverseExtras : List[Elem] = Nil) extends Element[Season]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("season", cxtras(new Text(this.number.toString)):_*)

        case class Subtitle(text : String, reverseExtras : List[Elem] = Nil) extends Element[Subtitle]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("subtitle", cxtras(new Text(this.text)):_*)

        case class Summary(text : String, reverseExtras : List[Elem] = Nil) extends Element[Summary]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("summary", cxtras(new Text(this.text)):_*)

        case class Title(title : String, reverseExtras : List[Elem] = Nil) extends Element[Title]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("title", cxtras(new Text(this.title)):_*)

        case class Type(validType : Itunes.ValidPodcastType, reverseExtras : List[Elem] = Nil) extends Element[Type]:
            override def withExtra(elem: Elem) = this.copy(reverseExtras = elem :: reverseExtras)
            override def toElem : Elem = ielem("type", cxtras(new Text(this.validType.toString)):_*)

trait Element[T <: Element[T]]:
    self : T =>
    val reverseExtras : List[Elem]

    protected def cxtras()                                : immutable.Seq[Node] = reverseExtras.reverse
    protected def cxtras( child : Node )                  : immutable.Seq[Node] = child :: reverseExtras.reverse
    protected def cxtras( children : immutable.Seq[Node]) : immutable.Seq[Node] = children.toList ::: reverseExtras.reverse

    def withExtra(elem : Elem)          : T
    def withExtra(element : Element[?]) : T = withExtra(element.toElem)

    def toElem : Elem

    def asXmlText( pp : PrettyPrinter ) : String =
        val noXmlDeclarationPretty = pp.format(this.toElem)
        s"<?xml version='1.0' encoding='UTF-8'?>\n${noXmlDeclarationPretty}"

    lazy val asXmlText : String = asXmlText( new PrettyPrinter(120,2) )

    lazy val bytes : immutable.Seq[Byte] = immutable.ArraySeq.ofByte(asXmlText.getBytes(scala.io.Codec.UTF8.charSet))






