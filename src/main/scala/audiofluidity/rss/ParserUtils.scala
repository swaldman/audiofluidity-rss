package audiofluidity.rss

import audiofluidity.rss.util.defaultNamespaceUri

import scala.annotation.tailrec
import scala.xml.{MetaData,Node,NodeSeq,Elem,PrefixedAttribute,UnprefixedAttribute}
import scala.xml.Attribute

object ParserUtils extends ParserUtils:
  val ParserForKind : Map[Element.Kind,Element.Parser[?]] =
    val ElementParsers : List[Element.Parser[?]] =
      Element.Atom.Link          ::
      Element.Atom.Published     ::
      Element.Atom.Summary       ::
      Element.Atom.Title         ::
      Element.Atom.Updated       ::
      Element.DublinCore.Creator ::
      Element.Iffy.Diff          ::
      Element.Iffy.HintAnnounce  ::
      Element.Iffy.Initial       ::
      Element.Iffy.Policy        ::
      Element.Iffy.Provenance    ::
      Element.Iffy.Restriction   ::
      Element.Iffy.Revision      ::
      Element.Iffy.Synthetic     ::
      Element.Iffy.Type          ::
      Element.Iffy.Uid           ::
      Element.Iffy.Update        ::
      Nil
    ElementParsers
      .map( parser => (parser.kind, parser) )
      .toMap

trait ParserUtils:
  private val UnknownNamespace = Namespace(Some("unknown"), "unknown:unknown")

  def attemptFillInReverseExtras( reverseExtras : List[Element.Extra] )( using pconfig : Parser.Config ) : ( Seq[String], List[Element.Extra] )=
    val chunkyUnreversed =
      reverseExtras.foldLeft( Tuple2( Nil : List[Seq[String]], Nil : List[Element.Extra] ) ): (accum, next) =>
        val ( warnings, extra ) = attemptFillInExtra( next )
        ( warnings :: accum(0), extra :: accum(1) )
    ( chunkyUnreversed(0).reverse.toVector.flatten, chunkyUnreversed(1).reverse )

  def attemptFillInExtra( extra : Element.Extra )( using pconfig : Parser.Config ) : ( Seq[String], Element.Extra ) =
    extra.mbSourceElement match
      case Some(_) => ( Nil, extra )
      case None    => attemptFillInExtra( extra.elem )

  def attemptFillInExtra( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], Element.Extra ) =
    val mbKind = Element.Kind.forElem( elem )
    mbKind match
      case None =>
        val warnings = Seq("Could not fill-in extra for elem, could not recognize its kind: " + elem)
        ( warnings, Element.Extra(elem) )
      case Some( kind ) =>
        val mbParser = ParserUtils.ParserForKind.get( kind )
        mbParser match
          case Some( parser ) =>
            val ( ws, mbElement ) = parser.fromChecked( elem )
            mbElement match
              case Some( Tuple2(_, element) ) => ( ws, Element.Extra( Some(element), elem ) )
              case None =>
                val warnings = ws :+ ("Could not fill-in extra for elem, parse failed: " + elem)
                ( warnings, Element.Extra(elem) )
          case None =>
            val warnings = Seq("Could not fill-in extra elem, no suitable parser found for kind: " + kind)
            ( warnings, Element.Extra(elem) )

  def allChildElemsAsReverseExtras( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], List[Element.Extra] ) =
    val childElems = elem.child.collect{ case e : Elem => e }
    childElems.foldLeft( Tuple2( Vector.empty[String], Nil : List[Element.Extra] ) ): (accum, next) =>
      val ( warnings, extra ) = attemptFillInExtra( elem )
      ( accum(0) ++ warnings, extra :: accum(1) )

  def reverseExtrasExcept( nonExtra : Vector[Elem] )( nlist : List[Node] )( using pconfig : Parser.Config ) : ( Seq[String], List[Element.Extra] ) =
    _reverseExtrasExcept( nonExtra, Vector.empty[String], Nil )( nlist )

  @tailrec
  private def _reverseExtrasExcept( nonExtra : Vector[Elem], warnings : Vector[String], accum : List[Element.Extra] )( nlist : List[Node] )( using pconfig : Parser.Config ) : ( Seq[String], List[Element.Extra] ) =
    nlist match
      case Nil => ( warnings, accum )
      case next :: rest =>
        next match
          case elem : Elem =>
            val loc = nonExtra.indexOf(elem)
            loc match
              case -1 =>
                val (ws, extra) = attemptFillInExtra( elem )
                _reverseExtrasExcept( nonExtra, warnings++ws, extra::accum )( rest )
              case n =>
                val excised = nonExtra.take(n) ++ nonExtra.drop(n+1)
                _reverseExtrasExcept( excised, warnings, accum )( rest )
          case _ =>
            _reverseExtrasExcept( nonExtra, warnings, accum )( rest )

  // XXX: We're not using pconfig yet, but eventually, when we've implemented a lot of parsers,
  //      we'll try to fill-in the first part of the Element.Extras we create, and it will matter there
  def childElemsAsReverseExtrasExcept( nonExtra : Vector[Elem] )( elem : Elem )( using pconfig : Parser.Config ) : ( Seq[String], List[Element.Extra] ) =
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
