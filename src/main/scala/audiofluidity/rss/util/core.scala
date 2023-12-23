package audiofluidity.rss.util

import scala.util.Try

import java.time.{Instant,ZonedDateTime}
import java.time.format.*
import java.time.temporal.ChronoField.*
import java.time.chrono.IsoChronology
import scala.jdk.CollectionConverters._
import java.time.temporal.TemporalAccessor

import scala.xml.*

private val RssDateTimeFormatter = DateTimeFormatter.RFC_1123_DATE_TIME

// see https://stackoverflow.com/questions/45829799/java-time-format-datetimeformatter-rfc-1123-date-time-fails-to-parse-time-zone-n
private val LenientRssDateTimeFormatter =
  val dow = Map(1L->"Mon",2L->"Tue",3L->"Wed",4L->"Thu",5L->"Fri",6L->"Sat",7L->"Sun").map( (k,v) => (k.asInstanceOf[java.lang.Long],v) )
  val moy = Map(1L->"Jan",2L->"Feb",3L->"Mar",4L->"Apr",5L->"May",6L->"Jun",7L->"Jul",8L->"Aug",9L->"Sep",10L->"Oct",11L->"Nov",12L->"Dec").map( (k,v) => (k.asInstanceOf[java.lang.Long],v) )
  new DateTimeFormatterBuilder()
    .parseCaseInsensitive()
    .parseLenient()
    .optionalStart()
    .appendText(DAY_OF_WEEK, dow.asJava)
    .appendLiteral(", ")
    .optionalEnd()
    .appendValue(DAY_OF_MONTH, 1, 2, SignStyle.NOT_NEGATIVE)
    .appendLiteral(' ')
    .appendText(MONTH_OF_YEAR, moy.asJava)
    .appendLiteral(' ')
    .appendValue(YEAR, 4)  // 2 digit year not handled
    .appendLiteral(' ')
    .appendValue(HOUR_OF_DAY, 2)
    .appendLiteral(':')
    .appendValue(MINUTE_OF_HOUR, 2)
    .optionalStart()
    .appendLiteral(':')
    .appendValue(SECOND_OF_MINUTE, 2)
    .optionalEnd()
    .appendLiteral(' ')
    // difference from RFC_1123_DATE_TIME: optional offset OR zone ID
    .optionalStart()
    .appendZoneText(TextStyle.SHORT)
    .optionalEnd()
    .optionalStart()
    .appendOffset("+HHMM", "GMT")
    // use the same resolver style and chronology
    .toFormatter().withResolverStyle(ResolverStyle.SMART).withChronology(IsoChronology.INSTANCE)

private def _attemptLenientParsePubDate( str : String ) : Try[TemporalAccessor] =
  (Try(RssDateTimeFormatter.parse(str)) orElse Try(LenientRssDateTimeFormatter.parse(str)))

def attemptLenientParsePubDateToInstant( str : String ) : Try[Instant] =
  _attemptLenientParsePubDate( str ).map( Instant.from )

def attemptLenientParsePubDate( str : String ) : Try[ZonedDateTime] =
  _attemptLenientParsePubDate( str ).map( ZonedDateTime.from )

def formatPubDate( zdt : ZonedDateTime ) : String = RssDateTimeFormatter.format( zdt )

def singleElementRss( rssElem : Elem, retainGuid : String ) : Either[String,Elem] =
  if rssElem.label != "rss" then
    Left("Base element not RSS element: " + rssElem)
  else
    val channelElems = rssElem.child.collect { case elem : Elem if elem.label == "channel" => elem }
    if channelElems.size != 1 then
      Left("Should contain precisely one channele element, contains " + channelElems.size)
    else
      val channelElem = channelElems.head
      def hasGuid( itemElem : Elem, guid : String ) : Boolean =
        val guidElems = itemElem \ "guid"
        guidElems.exists( _.text.trim() == guid )
      val filteredChildren = channelElem.child.filter: node =>
        node match
          case elem : Elem if elem.label == "item" && !hasGuid(elem, retainGuid) => false
          case _ => true
      val filteredChannelElem =
        channelElem.copy( child = filteredChildren )
      val newRssChildren = rssElem.child.map: node =>
        node match
          case elem : Elem if elem.label == "channel" => filteredChannelElem
          case other => other
      Right( rssElem.copy( child = newRssChildren ) )
