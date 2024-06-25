package audiofluidity.rss

import audiofluidity.rss.util.defaultNamespaceUri

import scala.annotation.tailrec
import scala.xml.{Node,NodeSeq,Elem}
import scala.xml.MetaData
import scala.xml.Attribute

object ParserUtils extends ParserUtils
trait ParserUtils:
  /*
  // XXX: We're not using retainParse yet, but eventually, when we've implemented a lot of parsers,
  //      we'll try to fill-in the first part of the Element.Extras we create, and it will matter there
  def reverseExtrasBesides( expected : String | (Namespace, String )* )( nseq : NodeSeq, retainParsed : Boolean ) : List[Element.Extra] =
    nseq.foldLeft( List.empty[Element.Extra] ): ( accum, next ) =>
      def hits( expectedItem : String | (Namespace, String), elem : Elem ) =
        expectedItem match
          case freeLabel : String                 => elem.label == freeLabel && defaultNamespaceUri(elem.scope) == None
          case ( ns : Namespace, label : String ) => elem.label == label && ns.belongsLenient(elem)

      next match
        case elem : Elem =>
          if expected.exists( expectedItem => hits( expectedItem, elem ) ) then accum // expected, not an extra!
          else Element.Extra( None, elem ) :: accum
        case _ => accum

  def childElemsBesidesAsReverseExtras( expected : String | (Namespace, String )* )( elem : Elem, retainParsed : Boolean ) : List[Element.Extra] = reverseExtrasBesides( expected* )( elem.child, retainParsed )
  def allChildElemsAsReverseExtras( elem : Elem, retainParsed : Boolean ) : List[Element.Extra] = reverseExtrasBesides()( elem.child, retainParsed )
  */

  private val UnknownNamespace = Namespace(Some("unknown"), "unknown:unknown")

  // XXX: We're not using retainParse yet, but eventually, when we've implemented a lot of parsers,
  //      we'll try to fill-in the first part of the Element.Extras we create, and it will matter there
  def allChildElemsAsReverseExtras( elem : Elem, retainParsed : Boolean ) : List[Element.Extra]
    = elemsBeyondAsReverseExtras()( elem.child.toList, retainParsed )

  // XXX: We're not using retainParse yet, but eventually, when we've implemented a lot of parsers,
  //      we'll try to fill-in the first part of the Element.Extras we create, and it will matter there
  def childElemsBeyondAsReverseExtras( expected : (Tuple2[Tuple2[Option[Namespace],String],Int]|Tuple2[Option[Namespace],String])* )( elem : Elem, retainParsed : Boolean ) : List[Element.Extra]
    = elemsBeyondAsReverseExtras(expected*)( elem.child.toList, retainParsed )


  // XXX: We're not using retainParse yet, but eventually, when we've implemented a lot of parsers,
  //      we'll try to fill-in the first part of the Element.Extras we create, and it will matter there
  def elemsBeyondAsReverseExtras( expected : (Tuple2[Tuple2[Option[Namespace],String],Int]|Tuple2[Option[Namespace],String])* )( nlist : List[Node], retainParsed : Boolean ) : List[Element.Extra] =
    val _expected =
      expected.map( (arg : Tuple2[Tuple2[Option[Namespace],String],Int] | Tuple2[Option[Namespace],String]) =>
          arg match
            case full @    ( _, _ : Int )                       => (full).asInstanceOf[Tuple2[Tuple2[Option[Namespace],String],Int]]                    // i'd hoped we could infer this type, but apparently not
            case partial @ ( _ : Option[Namespace], _ : String) => (partial -> Int.MaxValue).asInstanceOf[Tuple2[Tuple2[Option[Namespace],String],Int]] // i'd hoped we could infer this type, but apparently not
        )
        .toMap
    val expectedNamespaces = _expected.keySet.map( _(0) ).flatten
    _elemsBeyondAsReverseExtras(Nil,expectedNamespaces,_expected)(nlist, retainParsed)

  @tailrec
  private def _elemsBeyondAsReverseExtras(accum : List[Element.Extra], expectedNamespaces : Set[Namespace], expected : Map[Tuple2[Option[Namespace],String],Int] )( nlist : List[Node], retainParsed : Boolean ) : List[Element.Extra] =
    if nlist.isEmpty then
      accum
    else
      val next :: rest = (nlist : @unchecked) // we've already guarded aginst the empty case
      next match
        case e : Elem =>
          val namespace =
            val direct = expectedNamespaces.find( _.belongsLenient(e) )
            direct.orElse:
              if e.label == null && defaultNamespaceUri(e.scope) == None then None
              else Some(UnknownNamespace)
          val key = Tuple2( namespace, e.label )
          expected.get(key) match
            case Some(i) =>
              val newExpected =
                if i > 1 then expected + Tuple2(key, i-1)
                else expected - key
              _elemsBeyondAsReverseExtras(accum, expectedNamespaces, newExpected)(rest, retainParsed)
            case None =>
              val extra = Element.Extra(e)
              _elemsBeyondAsReverseExtras(extra :: accum, expectedNamespaces, expected)( rest, retainParsed )
        case other =>
          _elemsBeyondAsReverseExtras(accum, expectedNamespaces, expected)(rest, retainParsed) // we ignore/filter any non-Elems

  /**
    * @param fullKeys unprefixed keys or colon delimited prefix:key
    * @return
    */
  def attributesBeyond( fullKeys : String* )( md : MetaData ) : MetaData =
    val expected =
      fullKeys.map { fk =>
        val colonIndex = fk.lastIndexOf(':')
        if colonIndex >= 0 then
          ( fk.substring(0,colonIndex), fk.substring(colonIndex + 1) )
        else
          ( null, fk )
      }.toSet
    _attributesBeyond(expected)(md)

  def _attributesBeyond( expected : Set[(String,String)] )( md : MetaData ) : MetaData =
    md.filter: item =>
      item match
        case attr : Attribute =>
          val tup = ( attr.pre, attr.key ) // attr.pre is null if attr is unprefixed
          !expected(tup)
        case _ => false


  def getAttr( md : MetaData )( key : String ) : Option[String] =
    Option( md( key ) ).map( _.text.trim )
