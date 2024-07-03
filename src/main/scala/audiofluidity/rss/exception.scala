package audiofluidity.rss

class AudiofluidityRssException( msg : String, cause : Throwable = null ) extends Exception( msg, cause )
class BadAtomXml( msg : String, cause : Throwable = null ) extends AudiofluidityRssException( msg, cause )
class ConflictingNamespaces( msg : String, cause : Throwable = null ) extends AudiofluidityRssException( msg, cause )
class IncompleteNamespace( msg : String, cause : Throwable = null ) extends AudiofluidityRssException( msg, cause )
class UnsupportedNamespace( msg : String, cause : Throwable = null ) extends AudiofluidityRssException( msg, cause )

object UnparsableSynthetic:
  def message( parseWarnings : Seq[String], unknownType: Option[String] ) : String =
    val pwComponents =
      if parseWarnings.nonEmpty then
        "\tParse Warnings ->" ::
        parseWarnings.map( w => s"\t\t$w" ).toList
      else
        Nil

    val components =
      "UnparsableSynthetic:"                                  ::
      pwComponents                                            :::
      unknownType.map( ut => s"\tUnknown Type: ${ut}" ).toList

    components.mkString( System.lineSeparator )

case class UnparsableSynthetic( parseWarnings : Seq[String], unknownType: Option[String] = None )( cause : Throwable = null ) extends AudiofluidityRssException( UnparsableSynthetic.message( parseWarnings, unknownType ), cause )

