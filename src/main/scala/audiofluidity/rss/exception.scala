package audiofluidity.rss

class AudiofluidityRssException( msg : String, cause : Throwable = null ) extends Exception( msg, cause )

class ConflictingNamespaces( msg : String, cause : Throwable = null ) extends AudiofluidityRssException( msg, cause )
class BadAtomXml( msg : String, cause : Throwable = null ) extends AudiofluidityRssException( msg, cause )
