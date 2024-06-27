package audiofluidity.rss

import scala.xml.Elem

object Parser:
  object Fixer:
    def noFix[T <: Element[T]] : Fixer[T] = (_, _) => ( Nil, None )
    object Finder:
      case class Mapping[T <: Element[T]]( key : Element.Parser[T], value : Fixer[T] )
      def fromMappings( mappings : Seq[Mapping[?]] ) : Finder = new Finder:
        private val map : Map[Any,Any] = Map.from[Any,Any]( mappings.map( m => (m.key,m.value) ) )
        def find[T <: Element[T]]( eparser : Element.Parser[T] ) : Option[Fixer[T]] =
          map.get(eparser).map( _.asInstanceOf[Fixer[T]] )
    trait Finder:
      def find[T <: Element[T]]( eparser : Element.Parser[T] ) : Option[Fixer[T]]
  type Fixer[T <: Element[T]] = (Elem, Parser.Config) => ( Seq[String], Option[T] )
  case class Config (
    fixers       : Fixer.Finder,
    retainParsed : Element.Kinds
  )
