package audiofluidity.rss

import utest.*
import java.time.{ZonedDateTime,ZoneId}
import scala.xml.Null

object ElementTests extends TestSuite:
  val TestZonedDateTime = ZonedDateTime.of(2023, 10, 27, 19, 0, 0, 0, ZoneId.of("UTC"))

  // atom:published / atom:updated render via ISO_INSTANT and reparse to a "Z" offset zone,
  // so fixtures for those must already carry ZoneOffset.UTC to compare equal after a round trip.
  val TestZuluDateTime = ZonedDateTime.of(2023, 10, 27, 19, 0, 0, 0, ZoneId.of("Z"))

  val SourceFeedUrl = "https://source.example.com/feed.rss"

  /**
    * Render an element, hand the rendering back to its own parser, and return what comes out.
    * Fails if the parser rejects its own renderer's output or emits any warning.
    */
  def reparse[T <: Element[T]]( parser : Element.Parser[T] )( element : T ) : T =
    val ( warnings, mbParsed ) = parser.maybeFrom( element.toElem )
    assert( warnings.isEmpty )
    mbParsed match
      case Some( ( _, parsed ) ) => parsed
      case None => throw new java.lang.AssertionError( s"${parser} could not reparse its own rendering: ${element.toElem}" )

  /** Asserts that rendering then reparsing an element reproduces it exactly. */
  def roundTrips[T <: Element[T]]( parser : Element.Parser[T] )( element : T ) : Unit =
    reparse( parser )( element ) ==> element

  val TestLinkA = Element.Atom.Link( href = "https://a.example.com/feed.rss", rel = Some( Element.Atom.LinkRelation.via ) )
  val TestLinkB = Element.Atom.Link( href = "https://b.example.com/feed.rss", rel = Some( Element.Atom.LinkRelation.via ) )
  val TestLinkC = Element.Atom.Link( href = "https://c.example.com/feed.rss", rel = Some( Element.Atom.LinkRelation.via ) )

  val TestInitial =
    Element.Iffy.Initial(
      title     = Some( Element.Atom.Title( "An initial title." ) ),
      link      = Some( Element.Atom.Link( href = "https://example.com/item" ) ),
      guid      = Some( Element.Iffy.Uid( "urn:uuid:0000-0000" ) ),
      published = Some( Element.Atom.Published( TestZuluDateTime ) ),
      creators  = Seq( Element.DublinCore.Creator( "Jane Doe" ) )
    )

  val TestUpdate =
    Element.Iffy.Update(
      updated  = Element.Atom.Updated( TestZuluDateTime ),
      summary  = Some( Element.Atom.Summary( "What changed." ) ),
      revision = Some( Element.Iffy.Revision( "https://example.com/revision" ) ),
      diff     = Some( Element.Iffy.Diff( "https://example.com/diff" ) ),
      title    = Some( Element.Atom.Title( "An updated title." ) ),
      creators = Seq( Element.DublinCore.Creator( "Jane Doe" ) ),
      initial  = Some( TestInitial )
    )

  val tests = Tests:
    test("PubDateFormat"):
      val expected =
        """|<?xml version='1.0' encoding='UTF-8'?>
           |<pubDate>Fri, 27 Oct 2023 19:00:00 GMT</pubDate>""".stripMargin
      Element.PubDate( TestZonedDateTime ).asXmlText ==> expected

    // Every element that has a parser must render what that parser reads. Six of the seven bugs
    // fixed for 0.1.4 were violations of exactly this property, each invisible without it.
    test("parsersRoundTripTheirOwnRenderings"):
      import Element.{Atom,DublinCore,Iffy}

      roundTrips( Atom.Link ):
        Atom.Link(
          href     = "https://example.com/feed.rss",
          rel      = Some( Atom.LinkRelation.self ),
          `type`   = Some( "application/rss+xml" ),
          hreflang = Some( LanguageCode.EnglishUnitedStates ),
          title    = Some( "A Feed" ),
          length   = Some( 1024L )
        )
      roundTrips( Atom.Published )( Atom.Published( TestZuluDateTime ) )
      roundTrips( Atom.Summary   )( Atom.Summary( "A summary." ) )
      roundTrips( Atom.Title     )( Atom.Title( "A title." ) )
      roundTrips( Atom.Updated   )( Atom.Updated( TestZuluDateTime ) )

      roundTrips( DublinCore.Creator )( DublinCore.Creator( "Jane Doe" ) )

      roundTrips( Iffy.Diff        )( Iffy.Diff( "https://example.com/diff" ) )
      roundTrips( Iffy.Policy      )( Iffy.Policy( "Always" ) )
      roundTrips( Iffy.Restriction )( Iffy.Restriction() )
      roundTrips( Iffy.Revision    )( Iffy.Revision( "https://example.com/revision" ) )
      roundTrips( Iffy.Type        )( Iffy.Type( "UpdateAnnouncement" ) )
      roundTrips( Iffy.Uid         )( Iffy.Uid( "urn:uuid:0000-0000" ) )

      roundTrips( Iffy.HintAnnounce ):
        Iffy.HintAnnounce( Iffy.Policy( "Always" ), Some( Iffy.Restriction() ) )
      roundTrips( Iffy.Synthetic ):
        Iffy.Synthetic( Some( Iffy.Type( "UpdateAnnouncement" ) ) )
      roundTrips( Iffy.Initial )( TestInitial )
      roundTrips( Iffy.Update  )( TestUpdate )

    // iffy:uid used to render under a label its own parser did not match, so a rendered
    // iffy:initial lost its guid on reparse -- which broke Synthetic.cumulate downstream.
    test("initialRetainsUidThroughRoundTrip"):
      reparse( Element.Iffy.Initial )( TestInitial ).guid ==> Some( Element.Iffy.Uid( "urn:uuid:0000-0000" ) )

    // iffy:update parsed atom:title but never rendered it.
    test("updateRetainsTitleThroughRoundTrip"):
      reparse( Element.Iffy.Update )( TestUpdate ).title ==> Some( Element.Atom.Title( "An updated title." ) )

    // allChildElemsAsReverseExtras used to fill in the parent element rather than each child,
    // yielding self-referential extras. Only shows up when the element actually has child elems.
    test("unrecognizedChildrenBecomeExtrasOfThemselves"):
      val withExtra = Element.Iffy.Type( "UpdateAnnouncement" ).withExtra( Element.Iffy.Restriction() )
      val parsed    = reparse( Element.Iffy.Type )( withExtra )
      parsed ==> withExtra
      parsed.extraElements ==> List( Element.Iffy.Restriction() )

    // iffy:item-ref computed its attributes and then discarded them. It has no parser,
    // so assert against the rendering directly.
    test("itemRefRendersItsAttributes"):
      val attributes = Element.Iffy.ItemRef( "https://example.com/item", Some("text/html"), Some("urn:uuid:1") ).toElem.attributes
      ParserUtils.getAttr( attributes )( "href" ) ==> Some( "https://example.com/item" )
      ParserUtils.getAttr( attributes )( "type" ) ==> Some( "text/html" )
      ParserUtils.getAttr( attributes )( "guid" ) ==> Some( "urn:uuid:1" )

    // The iffy:provenance parser dropped shape, which also meant Provenance.apply never got to
    // collapse directly nested merges on the parse path. Constructed with `new` to bypass that
    // collapsing, so the nesting actually survives into the rendering under test.
    test("provenanceParsesShapeAndCollapsesNestedMerges"):
      import Element.Iffy.Provenance
      val innerMerge = Provenance( Seq( TestLinkB, TestLinkC ), shape = Some( Provenance.Shape.merge ) )
      val nested     = new Provenance( Seq( TestLinkA, innerMerge ), Some( Provenance.Shape.merge ), Nil, Nil, Null, None )

      // precondition: `new` really did bypass the collapsing, so the rendering is nested
      nested.linksAndProvenances.size ==> 2

      val parsed = reparse( Provenance )( nested )
      parsed.shape                          ==> Some( Provenance.Shape.merge )
      parsed.linksAndProvenances            ==> Seq( TestLinkA, TestLinkB, TestLinkC )

    test("provenanceRoundTripIsIdempotent"):
      import Element.Iffy.Provenance
      val once = Provenance( Seq( TestLinkA, TestLinkB ), shape = Some( Provenance.Shape.merge ) )
      roundTrips( Provenance )( once )

    // Synthetic.computeProvenance's isSimpleSourceLink recursed on its own argument, so this
    // whole branch was a StackOverflowError whenever any announcement carried a provenance.
    test("computeProvenanceMergesAnnouncementProvenances"):
      val announcement = Synthetic.UpdateAnnouncement( TestUpdate, provenance = Some( Element.Iffy.Provenance( Seq( TestLinkA ) ) ) )
      val merged = Synthetic.UpdateCumulation.computeProvenance( SourceFeedUrl, None, Seq( announcement ) )
      merged.map( _.shape )                     ==> Some( Some( Element.Iffy.Provenance.Shape.merge ) )
      merged.map( _.linksAndProvenances.size )  ==> Some( 2 )

    // ...and a provenance that IS just the source link must be filtered, not duplicated.
    test("computeProvenanceFiltersRedundantSourceProvenance"):
      val sourceLink   = Element.Atom.Link( href = SourceFeedUrl, rel = Some( Element.Atom.LinkRelation.via ) )
      val announcement = Synthetic.UpdateAnnouncement( TestUpdate, provenance = Some( Element.Iffy.Provenance( Seq( sourceLink ) ) ) )
      val merged = Synthetic.UpdateCumulation.computeProvenance( SourceFeedUrl, None, Seq( announcement ) )
      merged.map( _.linksAndProvenances.size ) ==> Some( 1 )
