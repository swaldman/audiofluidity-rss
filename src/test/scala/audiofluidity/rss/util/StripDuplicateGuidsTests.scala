package audiofluidity.rss.util

import utest.*
import scala.xml.*

object StripDuplicateGuidsTests extends TestSuite:

  def item( guid : String, title : String ) : Elem =
    <item><guid>{guid}</guid><title>{title}</title></item>

  def feed( channelChildren : Node* ) : Elem =
    <rss version="2.0"><channel><title>T</title>{channelChildren}<link>http://x/</link></channel></rss>

  def titles( rss : Elem ) : Seq[String] =
    (rss \ "channel" \ "item").map( i => (i \ "title").text )

  def right( e : Either[RssParseFailure,Elem] ) : Elem =
    e match
      case Right(elem) => elem
      case Left(f)     => throw new RuntimeException( "Unexpected failure: " + f )

  val tests = Tests:

    test("noDuplicatesReturnsEquivalentDocument"):
      val in = feed( item("a","1"), item("b","2") )
      var called = false
      val out = right( stripItemsWithDuplicateGuids( in, ies => { called = true; ies.toElemSeq.head } ) )
      assert( !called )
      out ==> in

    test("retainFirstKeepsFirstOfEachDuplicateGroupInPlace"):
      val in = feed( item("a","a1"), item("b","b1"), item("a","a2"), item("c","c1"), item("b","b2"), item("a","a3") )
      val out = right( retainFirstItemsWithDuplicateGuids( in ) )
      titles( out ) ==> Seq("a1","b1","c1")

    test("chooseReceivesDuplicatesInDocumentOrder"):
      val in = feed( item("a","a1"), item("b","b1"), item("a","a2"), item("a","a3") )
      var seen = Seq.empty[Seq[String]]
      stripItemsWithDuplicateGuids( in, ies => { seen = seen :+ ies.toElemSeq.map( i => (i \ "title").text ); ies.toElemSeq.head } )
      seen ==> Seq( Seq("a1","a2","a3") )

    test("chosenNonFirstStaysInItsOwnPosition"):
      val in = feed( item("a","a1"), item("b","b1"), item("a","a2"), item("c","c1") )
      val out = right( stripItemsWithDuplicateGuids( in, _.toElemSeq.last ) )
      titles( out ) ==> Seq("b1","a2","c1")

    test("foreignChosenReplacesFirstDuplicate"):
      val in = feed( item("a","a1"), item("b","b1"), item("a","a2") )
      val merged = item("a","merged")
      val out = right( stripItemsWithDuplicateGuids( in, _ => merged ) )
      titles( out ) ==> Seq("merged","b1")

    test("structurallyIdenticalDuplicatesCollapseToOne"):
      val in = feed( item("a","same"), item("a","same"), item("b","x") )
      val out = right( retainFirstItemsWithDuplicateGuids( in ) )
      titles( out ) ==> Seq("same","x")

    test("guidWhitespaceIsTrimmed"):
      val in = feed( item("a","a1"), item("  a\n","a2") )
      titles( right( retainFirstItemsWithDuplicateGuids( in ) ) ) ==> Seq("a1")

    test("itemsWithoutGuidAreNeverTouched"):
      val noGuid = <item><title>ng</title></item>
      val in = feed( noGuid, <item><title>ng</title></item>, item("a","a1") )
      titles( right( retainFirstItemsWithDuplicateGuids( in ) ) ) ==> Seq("ng","ng","a1")

    test("nonItemChildrenAndAttributesPreserved"):
      val in = feed( item("a","a1"), <!-- hi -->, item("a","a2") )
      val out = right( retainFirstItemsWithDuplicateGuids( in ) )
      out.attribute("version").map(_.text) ==> Some("2.0")
      (out \ "channel" \ "title").text ==> "T"
      (out \ "channel" \ "link").text ==> "http://x/"
      (out \ "channel" \ "item").size ==> 1

    test("nonRssRootFails"):
      val in = <feed/>
      stripItemsWithDuplicateGuids( in, _.toElemSeq.head ) ==> Left( RssParseFailure.BaseElementNotRss(in) )

    test("zeroChannelsFails"):
      stripItemsWithDuplicateGuids( <rss/>, _.toElemSeq.head ) ==> Left( RssParseFailure.ZeroOrMultipleChannelElems(0) )

    test("multipleChannelsFails"):
      stripItemsWithDuplicateGuids( <rss><channel/><channel/></rss>, _.toElemSeq.head ) ==> Left( RssParseFailure.ZeroOrMultipleChannelElems(2) )

    // stripItemsWithDuplicatedGuid: the known-guid variant

    test("duplicatedGuidOnlyRepairsThatGuid"):
      val in = feed( item("a","a1"), item("b","b1"), item("a","a2"), item("b","b2") )
      val out = right( stripItemsWithDuplicatedGuid( in, "a", _.toElemSeq.head ) )
      titles( out ) ==> Seq("a1","b1","b2")

    test("duplicatedGuidChosenNonFirstStaysInPlace"):
      val in = feed( item("a","a1"), item("b","b1"), item("a","a2"), item("c","c1") )
      val out = right( stripItemsWithDuplicatedGuid( in, "a", _.toElemSeq.last ) )
      titles( out ) ==> Seq("b1","a2","c1")

    test("duplicatedGuidForeignChosenReplacesFirst"):
      val in = feed( item("a","a1"), item("b","b1"), item("a","a2") )
      val out = right( stripItemsWithDuplicatedGuid( in, "a", _ => item("a","merged") ) )
      titles( out ) ==> Seq("merged","b1")

    test("duplicatedGuidChooseSeesOnlyThatGuidInOrder"):
      val in = feed( item("a","a1"), item("b","b1"), item("a","a2"), item("a","a3") )
      var seen = Seq.empty[String]
      stripItemsWithDuplicatedGuid( in, "a", ies => { seen = ies.toElemSeq.map( i => (i \ "title").text ); ies.toElemSeq.head } )
      seen ==> Seq("a1","a2","a3")

    test("duplicatedGuidSingleItemIsNoOp"):
      val in = feed( item("a","a1"), item("b","b1") )
      var called = false
      val out = right( stripItemsWithDuplicatedGuid( in, "a", ies => { called = true; ies.toElemSeq.head } ) )
      assert( !called )
      out ==> in

    test("duplicatedGuidMissingFails"):
      stripItemsWithDuplicatedGuid( feed( item("a","a1") ), "zzz", _.toElemSeq.head ) ==> Left( RssParseFailure.GuidNotFound("zzz") )

    test("duplicatedGuidStructuralFailures"):
      val bad = <feed/>
      stripItemsWithDuplicatedGuid( bad, "a", _.toElemSeq.head ) ==> Left( RssParseFailure.BaseElementNotRss(bad) )
      stripItemsWithDuplicatedGuid( <rss/>, "a", _.toElemSeq.head ) ==> Left( RssParseFailure.ZeroOrMultipleChannelElems(0) )
