package audiofluidity.rss.util

import scala.util.Try

import java.time.Instant
import java.time.format.*
import java.time.temporal.ChronoField.*
import java.time.chrono.IsoChronology
import scala.jdk.CollectionConverters._

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

def attemptLenientParsePubDate( str : String ) : Try[Instant] =
  (Try(RssDateTimeFormatter.parse(str)) orElse Try(LenientRssDateTimeFormatter.parse(str))).map( Instant.from )

