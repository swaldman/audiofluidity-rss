package audiofluidity.rss

object Parser:
  case class Config (
    val retainParsed : Element.Kinds
  )
