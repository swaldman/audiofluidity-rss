package audiofluidity.rss

import scala.xml.Elem

object Parser:
  object Custom:
    def silentFail[T <: Element[T]] : Custom[T] = (_, _) => ( Nil, None )
    object Finder:
      case class Mapping[T <: Element[T]]( key : Element.Parser[T], value : Custom[T] )
      def fromMappings( mappings : Seq[Mapping[?]] ) : Finder = new Finder:
        private val map : Map[Any,Any] = Map.from[Any,Any]( mappings.map( m => (m.key,m.value) ) )
        def find[T <: Element[T]]( eparser : Element.Parser[T] ) : Option[Custom[T]] =
          map.get(eparser).map( _.asInstanceOf[Custom[T]] )
    trait Finder:
      def find[T <: Element[T]]( eparser : Element.Parser[T] ) : Option[Custom[T]]
  type Custom[T <: Element[T]] = (Elem, Parser.Config) => ( Seq[String], Option[(Elem,T)] )
  case class Config (
    preempts       : Custom.Finder,
    fallbacks      : Custom.Finder,
    retainParsed : Element.Kinds
  )
