package audiofluidity.rss.util

import utest.*
import java.time.{Instant,LocalDateTime,ZoneOffset}

object LenientRfc1123Tests extends TestSuite:

  // All five fixtures below name the same wall-clock moment, 12:31(:30) on 5 Aug 2026,
  // in one zone or another, so the expected instants differ only by the zone's offset.
  def instantAt( hour : Int, minute : Int, second : Int ) : Instant =
    LocalDateTime.of( 2026, 8, 5, hour, minute, second ).toInstant( ZoneOffset.UTC )

  def normalized( raw : String ) : String = LenientRfc1123.normalize( raw )

  def instant( raw : String ) : Instant = LenientRfc1123.parseOrThrow( raw ).toInstant

  val tests = Tests:

    // No day-of-week, one-digit day, no seconds, alphabetic zone: the TrailingAbbrev
    // branch of normalizeZone plus the optional-seconds branch of the Canonical formatter.
    test("noDayOfWeekOneDigitDayNoSeconds"):
      val raw = "5 Aug 2026 12:31 GMT"
      normalized( raw ) ==> "5 Aug 2026 12:31 +0000"
      instant( raw )    ==> instantAt( 12, 31, 0 )

    // Two-digit year (pivot to 2026, not 0026), an RFC 5322 comment to strip, a numeric
    // offset that must beat the "(EST)" comment, and a day-of-week that is flatly wrong
    // (5 Aug 2026 is a Wednesday) and so must be discarded rather than resolved against.
    test("twoDigitYearWithParentheticalCommentAndWrongDayOfWeek"):
      val raw = "Tue, 05 Aug 26 12:31:30 -0500 (EST)"
      normalized( raw ) ==> "05 Aug 2026 12:31:30 -0500"
      instant( raw )    ==> instantAt( 17, 31, 30 )
      LenientRfc1123.parseOrThrow( raw ).getOffset ==> ZoneOffset.ofHours( -5 )

    // Month before day: the MonthFirst transposition.
    test("monthFirstTransposition"):
      val raw = "Aug 05 2026 12:31:30 EDT"
      normalized( raw ) ==> "05 Aug 2026 12:31:30 -0400"
      instant( raw )    ==> instantAt( 16, 31, 30 )

    // Hyphen-separated date head, and "UT" rather than "UTC".
    test("hyphenatedDateHeadAndUtAbbreviation"):
      val raw = "05-Aug-2026 12:31:30 UT"
      normalized( raw ) ==> "05 Aug 2026 12:31:30 +0000"
      instant( raw )    ==> instantAt( 12, 31, 30 )

    // Not RFC 1123 at all. The date head does not match, so normalization touches only the
    // zone, producing "+0000", which appendZoneOrOffsetId will not take; the parse succeeds
    // on the second candidate, raw input against SpacedZone.
    test("isoLikeDateFallsThroughToSpacedZone"):
      val raw = "2026-08-05 12:31:30 UTC"
      instant( raw ) ==> instantAt( 12, 31, 30 )
      LenientRfc1123.parseOrThrow( raw ).toLocalDateTime ==> LocalDateTime.of( 2026, 8, 5, 12, 31, 30 )
