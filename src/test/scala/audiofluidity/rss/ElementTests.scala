package audiofluidity.rss

import utest.*
import java.time.{ZonedDateTime,ZoneId}

object ElementTests extends TestSuite:
  val TestZonedDateTime = ZonedDateTime.of(2023, 10, 27, 19, 0, 0, 0, ZoneId.of("UTC"))
  val tests = Tests:
    test("PubDateFormat"):
      val expected =
        """|<?xml version='1.0' encoding='UTF-8'?>
           |<pubDate>Fri, 27 Oct 2023 19:00:00 GMT</pubDate>""".stripMargin
      Element.PubDate( TestZonedDateTime ).asXmlText ==> expected
      
    
