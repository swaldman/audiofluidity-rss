package audiofluidity.rss.util

import java.time.*
import java.time.chrono.IsoChronology
import java.time.format.*
import java.time.temporal.ChronoField.*
import java.time.temporal.TemporalAccessor
import java.util.Locale
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/**
 * Lenient parser for RFC-1123 / RFC-822 / RFC-2822 style timestamps as they actually
 * appear in RSS `pubDate`, Atom `updated`, and HTTP `Date` headers.
 *
 * DESIGN
 * ------
 * All tolerance lives in `normalize`, a pure String => String function. The formatter
 * itself is strict.
 *
 * `parseLenient()` is deliberately NOT used. It relaxes declared field widths --
 * `appendValue(YEAR, 4)` under lenient parsing accepts 1..9 digits, so "26" silently
 * becomes year 0026 instead of failing. Malformed input becoming wrong output is worse
 * than malformed input becoming an exception. Keeping the formatter strict means every
 * deviation we tolerate is an explicit, testable line in the normalizer.
 *
 * Locale is pinned to ROOT and month names come from an explicit map, so behavior does
 * not vary with the host's default locale.
 *
 * (This begins as a claude-generated improvement on our earlier (stack-overflow derived) LenientRssDateTimeFormatter)
 */
object LenientRfc1123:

  final case class Config(
    /** Applied when the input carries no zone or offset at all. */
    defaultZone: ZoneId = ZoneOffset.UTC,
    /** 00..(pivot-1) -> 20xx, pivot..99 -> 19xx. RFC 2822 specifies 50. */
    twoDigitYearPivot: Int = 50,
    /** Overrides/additions to the abbreviation table, e.g. Map("IST" -> "+0100"). */
    zoneOverrides: Map[String, String] = Map.empty
  )

  object Config:
    val Default: Config = Config()

  // ---------------------------------------------------------------- zone table

  /**
   * Zone abbreviations resolved to FIXED offsets rather than ZoneIds.
   *
   * This is correct, not a shortcut: the abbreviations already encode DST themselves
   * ("EST" is always -0500, "EDT" always -0400), so there is no DST rule left to apply.
   * It also sidesteps `appendZoneText`, whose parsing is locale-sensitive.
   *
   * Entries marked AMBIGUOUS have multiple real-world meanings; the choice here is the
   * most common one in English-language feeds. Override via Config.zoneOverrides.
   */
  private val BaseZoneAbbrevs: Map[String, String] = Map(
    "UT"   -> "+0000", "UTC"  -> "+0000", "GMT"  -> "+0000", "Z" -> "+0000",
    "EST"  -> "-0500", "EDT"  -> "-0400",
    "CST"  -> "-0600", "CDT"  -> "-0500",   // AMBIGUOUS: China +0800, Cuba -0500
    "MST"  -> "-0700", "MDT"  -> "-0600",
    "PST"  -> "-0800", "PDT"  -> "-0700",
    "AKST" -> "-0900", "AKDT" -> "-0800",
    "HST"  -> "-1000", "HDT"  -> "-0900",
    "AST"  -> "-0400", "ADT"  -> "-0300",   // AMBIGUOUS: Arabia +0300
    "NST"  -> "-0330", "NDT"  -> "-0230",
    "BRT"  -> "-0300", "ART"  -> "-0300", "UYT" -> "-0300",
    "WET"  -> "+0000", "WEST" -> "+0100",
    "BST"  -> "+0100",                      // AMBIGUOUS: Bangladesh +0600
    "IST"  -> "+0530",                      // AMBIGUOUS: Ireland +0100, Israel +0200
    "CET"  -> "+0100", "CEST" -> "+0200",
    "MET"  -> "+0100", "MEST" -> "+0200",
    "EET"  -> "+0200", "EEST" -> "+0300",
    "MSK"  -> "+0300",
    "PKT"  -> "+0500", "ICT"  -> "+0700", "WIB" -> "+0700",
    "SGT"  -> "+0800", "HKT"  -> "+0800", "CHST" -> "+1000",
    "JST"  -> "+0900", "KST"  -> "+0900",
    "AWST" -> "+0800", "ACST" -> "+0930", "ACDT" -> "+1030",
    "AEST" -> "+1000", "AEDT" -> "+1100",
    "NZST" -> "+1200", "NZDT" -> "+1300"
  )

  private val Months3 = Vector(
    "jan", "feb", "mar", "apr", "may", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec"
  )

  private val MonthMap: java.util.Map[java.lang.Long, String] =
    Months3.zipWithIndex.map { (name, i) =>
      java.lang.Long.valueOf((i + 1).toLong) -> name.capitalize
    }.toMap.asJava

  // ------------------------------------------------------------ normalization

  private val ExoticSpace = "[\u00A0\u1680\u2000-\u200B\u2028\u2029\u202F\u205F\u3000]"

  // "Wed," / "Wednesday" / "Weds." / "Wed" with no comma at all
  private val DowPrefix =
    """(?i)^(?:mon|tues?|wed(?:nes)?|thur?s?|fri|sat|sun)[a-z]*\.?\s*,?\s*"""

  // "Aug 05 2026" -> day and month transposed
  private val MonthFirst = """(?i)^([a-z]{3,9})\.?[\s\-/]+(\d{1,2})(?=[\s,\-/])""".r

  // The date head: day, month name, year -- separators may be space, hyphen, or slash
  private val DateHead = """(?i)^(\d{1,2})[\s\-/]+([a-z]{3,9})\.?[\s,\-/]+(\d{2,4})\b""".r

  private val ParenComment = """\([^()]*\)""".r

  // Trailing "+HH:MM" / "+HHMM" / "+HH", optionally prefixed by GMT/UTC/UT
  private val TrailingOffset = """(?i)(?:GMT|UTC|UT)?\s*([+-])(\d{1,2}):?(\d{2})?\s*$""".r
  // Trailing alphabetic zone abbreviation
  private val TrailingAbbrev = """(?i)[\s,]+([a-z]{1,5})\s*$""".r
  // Trailing IANA zone id, e.g. "America/New_York"
  private val TrailingZoneId = """[\s,]+([A-Za-z]+(?:/[A-Za-z_+\-0-9]+)+)\s*$""".r

  /**
   * Rewrites real-world variants into the single canonical shape
   * `d MMM yyyy HH:mm[:ss] +HHMM`, which the strict formatter below accepts.
   *
   * Each step is independent and safe to test in isolation.
   */
  def normalize(raw: String, cfg: Config = Config.Default): String =
    var s = raw

    // 1. Whitespace hygiene: exotic spaces, tabs, newlines, runs.
    s = s.replaceAll(ExoticSpace, " ").replaceAll("""\s+""", " ").trim

    // 2. Strip RFC 5322 comments, e.g. "-0500 (Eastern Standard Time)".
    //    Looped to unwind nesting; bounded so pathological input can't spin.
    var i = 0
    while i < 4 && ParenComment.findFirstIn(s).isDefined do
      s = ParenComment.replaceAllIn(s, " ")
      i += 1
    s = s.replaceAll("""\s+""", " ").trim

    // 3. Drop the day-of-week entirely.
    //    It is redundant, and feeds get it wrong often enough that keeping it means
    //    SMART/STRICT resolution throws on a self-contradictory-but-recoverable date.
    s = s.replaceAll(DowPrefix, "")

    // 4. Transpose "Aug 05 2026" -> "05 Aug 2026".
    s = MonthFirst.replaceFirstIn(s, "$2 $1")

    // 5. Canonicalize the date head: 3-letter month, 4-digit year, single spaces.
    s = DateHead.findFirstMatchIn(s) match
      case Some(m) =>
        val day   = m.group(1)
        val mon   = m.group(2).toLowerCase(Locale.ROOT).take(3)
        val year  = expandYear(m.group(3), cfg.twoDigitYearPivot)
        s"$day ${mon.capitalize} $year" + s.substring(m.end)
      case None => s

    // 6. Zone tail -> numeric offset.
    s = normalizeZone(s, cfg)

    s.replaceAll("""\s+""", " ").trim
  end normalize

  /** RFC 2822 §4.3: 2-digit years pivot; 3-digit years get 1900 added. */
  private def expandYear(y: String, pivot: Int): String =
    y.length match
      case 4 => y
      case 3 => (y.toInt + 1900).toString
      case 2 => val n = y.toInt; if n < pivot then f"${2000 + n}%04d" else f"${1900 + n}%04d"
      case _ => y

  /**
   * Each branch is an Option: None means "this shape does not apply, try the next one".
   * Written as an orElse chain rather than early returns, both because `return` inside a
   * closure is a non-local return (no longer supported) and because the chain makes the
   * precedence -- numeric, then abbreviation, then IANA id -- the shape of the code.
   */
  private def normalizeZone(in: String, cfg: Config): String =
    val table = BaseZoneAbbrevs ++ cfg.zoneOverrides

    // Numeric offset wins -- it is unambiguous, so try it before any name.
    def numeric: Option[String] =
      TrailingOffset.findFirstMatchIn(in).map { m =>
        val sign = m.group(1)
        val hh   = f"${m.group(2).toInt}%02d"
        val mm   = Option(m.group(3)).getOrElse("00")
        in.substring(0, m.start).trim + s" $sign$hh$mm"
      }

    // Known abbreviation -> fixed offset. An *unknown* abbreviation yields None, so the
    // IANA branch below still gets its chance at the same tail.
    def abbrev: Option[String] =
      for
        m   <- TrailingAbbrev.findFirstMatchIn(in)
        off <- table.get(m.group(1).toUpperCase(Locale.ROOT))
      yield
        in.substring(0, m.start).trim + " " + off

    // IANA zone id -> offset valid on that date. Requires the head to parse first,
    // so we defer: leave the id in place and let the ZoneId branch of the chain take it.
    def ianaId: Option[String] =
      TrailingZoneId.findFirstMatchIn(in).filter(m => Try(ZoneId.of(m.group(1))).isSuccess).map { m =>
        // Resolve against the local part so DST is applied for the right date.
        val head = in.substring(0, m.start).trim
        Try {
          val ldt = LocalDateTime.parse(head, LocalPart)
          val off = ZoneId.of(m.group(1)).getRules.getOffset(ldt)
          head + " " + off.getId.replace(":", "").replace("Z", "+0000")
        }.getOrElse(in)
      }

    numeric.orElse(abbrev).orElse(ianaId).getOrElse(in)
  end normalizeZone

  // ---------------------------------------------------------------- formatters

  /** Date + time, no zone. Used internally for IANA-zone offset resolution. */
  private val LocalPart: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .parseCaseInsensitive()
      .appendValue(DAY_OF_MONTH, 1, 2, SignStyle.NOT_NEGATIVE)
      .appendLiteral(' ')
      .appendText(MONTH_OF_YEAR, MonthMap)
      .appendLiteral(' ')
      .appendValue(YEAR, 4, 9, SignStyle.EXCEEDS_PAD)
      .appendLiteral(' ')
      .appendValue(HOUR_OF_DAY, 1, 2, SignStyle.NOT_NEGATIVE)
      .appendLiteral(':')
      .appendValue(MINUTE_OF_HOUR, 1, 2, SignStyle.NOT_NEGATIVE)
      .optionalStart()
        .appendLiteral(':')
        .appendValue(SECOND_OF_MINUTE, 1, 2, SignStyle.NOT_NEGATIVE)
      .optionalEnd()
      .toFormatter(Locale.ROOT)
      .withResolverStyle(ResolverStyle.SMART)
      .withChronology(IsoChronology.INSTANCE)

  /** The canonical post-normalization shape, with the offset optional. */
  private val Canonical: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .parseCaseInsensitive()
      .append(LocalPart)
      .optionalStart()
        .appendFraction(NANO_OF_SECOND, 1, 9, true)
      .optionalEnd()
      .optionalStart()
        .appendLiteral(' ')
        .appendOffset("+HHMM", "+0000")
      .optionalEnd()
      .toFormatter(Locale.ROOT)
      .withResolverStyle(ResolverStyle.SMART)
      .withChronology(IsoChronology.INSTANCE)

  /** "2026-08-05 12:31:30 UTC" and friends -- space separator, zone name or offset. */
  private val SpacedZone: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .parseCaseInsensitive()
      .append(DateTimeFormatter.ISO_LOCAL_DATE)
      .appendLiteral(' ')
      .append(DateTimeFormatter.ISO_LOCAL_TIME)
      .optionalStart()
        .appendLiteral(' ')
        .appendZoneOrOffsetId()
      .optionalEnd()
      .toFormatter(Locale.ENGLISH)
      .withResolverStyle(ResolverStyle.SMART)
      .withChronology(IsoChronology.INSTANCE)

  /** Tried in order. Cheap/unambiguous first. Hoisted so nothing allocates per call. */
  private val Formatters: List[DateTimeFormatter] = List(
    Canonical,
    DateTimeFormatter.ISO_OFFSET_DATE_TIME,
    DateTimeFormatter.ISO_ZONED_DATE_TIME,
    DateTimeFormatter.ISO_INSTANT,
    SpacedZone,
    DateTimeFormatter.ISO_LOCAL_DATE_TIME,
    DateTimeFormatter.ISO_LOCAL_DATE
  )

  // -------------------------------------------------------------------- parse

  /**
   * Collapses whatever the formatter produced into a ZonedDateTime, applying
   * cfg.defaultZone only when the input genuinely carried no zone information.
   *
   * Note we do NOT use parseDefaulting(OFFSET_SECONDS, 0): that fills the field whenever
   * it is absent, including when a zone *name* was parsed but no numeric offset, which
   * would force UTC onto an otherwise correct timestamp.
   */
  private def toZoned(ta: TemporalAccessor, cfg: Config): Try[ZonedDateTime] =
    Try(ZonedDateTime.from(ta))
      .orElse(Try(OffsetDateTime.from(ta).toZonedDateTime))
      .orElse(Try(Instant.from(ta).atZone(cfg.defaultZone)))
      .orElse(Try(LocalDateTime.from(ta).atZone(cfg.defaultZone)))
      .orElse(Try(LocalDate.from(ta).atStartOfDay(cfg.defaultZone)))

  /** Parses a timestamp, tolerating the common real-world deviations. */
  def parse(raw: String, cfg: Config = Config.Default): Try[ZonedDateTime] =
    if raw == null || raw.isBlank then
      Failure(new DateTimeParseException("Blank timestamp", String.valueOf(raw), 0))
    else
      val normalized = normalize(raw, cfg)
      // Lazy: the first formatter that works stops the chain, so we never pay for
      // the DateTimeParseException stack traces of the formatters after it.
      val candidates = List(normalized, raw.trim).distinct
      candidates.iterator
        .flatMap(s => Formatters.iterator.map(f => (s, f)))
        .flatMap { (s, f) => Try(f.parse(s)).flatMap(toZoned(_, cfg)).toOption }
        .nextOption()
        .toRight(new DateTimeParseException(
          s"Unparseable timestamp: '$raw' (normalized to '$normalized')", raw, 0))
        .toTry

  def parseOrThrow(raw: String, cfg: Config = Config.Default): ZonedDateTime =
    parse(raw, cfg).get

  def parseOption(raw: String, cfg: Config = Config.Default): Option[ZonedDateTime] =
    parse(raw, cfg).toOption

end LenientRfc1123
