package audiofluidity.rss

import audiofluidity.rss.util.defaultNamespaceUri

import scala.annotation.tailrec
import scala.xml.{MetaData,Node,NodeSeq,Elem,PrefixedAttribute,UnprefixedAttribute}
import scala.xml.Attribute

object ParserUtils extends ParserUtils
trait ParserUtils:
  private val UnknownNamespace = Namespace(Some("unknown"), "unknown:unknown")

  // XXX: We're not using pconfig yet, but eventually, when we've implemented a lot of parsers,
  //      we'll try to fill-in the first part of the Element.Extras we create, and it will matter there
  def allChildElemsAsReverseExtras( elem : Elem )( using pconfig : Parser.Config ) : List[Element.Extra]
    = elem.child.collect{ case e : Elem => e }.foldLeft(Nil:List[Element.Extra])((accum,next) => Element.Extra(next)::accum)

  def reverseExtrasExcept( nonExtra : Vector[Elem] )( nlist : List[Node] )( using pconfig : Parser.Config ) : List[Element.Extra] =
    _reverseExtrasExcept( nonExtra, Nil )( nlist )

  @tailrec
  private def _reverseExtrasExcept( nonExtra : Vector[Elem], accum : List[Element.Extra] )( nlist : List[Node] )( using pconfig : Parser.Config ) : List[Element.Extra] =
    nlist match
      case Nil => accum
      case next :: rest =>
        next match
          case elem : Elem =>
            val loc = nonExtra.indexOf(elem)
            loc match
              case -1 => _reverseExtrasExcept( nonExtra, Element.Extra(elem)::accum )( rest )
              case  n =>
                val excised = nonExtra.slice(0,n) ++ nonExtra.drop(n+1)
                _reverseExtrasExcept( excised, accum )( rest )
          case _ =>
            _reverseExtrasExcept( nonExtra, accum )( rest )

  // XXX: We're not using pconfig yet, but eventually, when we've implemented a lot of parsers,
  //      we'll try to fill-in the first part of the Element.Extras we create, and it will matter there
  def childElemsAsReverseExtrasExcept( nonExtra : Vector[Elem] )( elem : Elem )( using pconfig : Parser.Config ) : List[Element.Extra] =
    reverseExtrasExcept(nonExtra)( elem.child.toList )

  /**
    * @param fullKeys unprefixed keys or colon delimited prefix:key
    * @return
    */
  def attributesExcept( fullKeys : String* )( md : MetaData ) : MetaData =
    val expected =
      fullKeys.map { fk =>
        val colonIndex = fk.lastIndexOf(':')
        if colonIndex >= 0 then
          ( fk.substring(0,colonIndex), fk.substring(colonIndex + 1) )
        else
          ( null, fk )
      }.toSet
    _attributesExcept(expected)(md)

  def _attributesExcept( expected : Set[(String,String)] )( md : MetaData ) : MetaData =
    md.filter: item =>
      item match
        case attr : Attribute =>
          val tup = ( attr.pre, attr.key ) // attr.pre is null if attr is unprefixed
          !expected(tup)
        case _ => false

  def prependAttribute( prefixOrNull : String, key : String, value : String, md : MetaData ) : MetaData =
    if prefixOrNull == null then
      new UnprefixedAttribute(key, value, md)
    else
      new PrefixedAttribute(prefixOrNull, key, value, md)

  def prependAttribute( key : String, value : String, md : MetaData ) : MetaData =
    prependAttribute(null, key, value, md)

  def getAttr( md : MetaData )( key : String ) : Option[String] =
    Option( md( key ) ).map( _.text.trim )

  def elemsOfKinds( kinds : Element.Kinds, nodes : Seq[Node] ) : Seq[Elem] =
    nodes.collect { case elem : Elem if Element.Kind.forElem(elem).fold(false)( kind => Element.Kinds.contains(kinds, kind) ) => elem }
