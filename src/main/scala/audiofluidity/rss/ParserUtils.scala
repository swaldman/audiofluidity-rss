package audiofluidity.rss

import audiofluidity.rss.util.defaultNamespaceUri

import scala.annotation.tailrec
import scala.xml.{Node,NodeSeq,Elem}
import scala.xml.MetaData
import scala.xml.Attribute

object ParserUtils extends ParserUtils
trait ParserUtils:
  private val UnknownNamespace = Namespace(Some("unknown"), "unknown:unknown")

  // XXX: We're not using pconfig yet, but eventually, when we've implemented a lot of parsers,
  //      we'll try to fill-in the first part of the Element.Extras we create, and it will matter there
  def allChildElemsAsReverseExtras( elem : Elem )( using pconfig : Parser.Config ) : List[Element.Extra]
    = elem.child.collect{ case e : Elem => e }.foldLeft(Nil:List[Element.Extra])((accum,next) => Element.Extra(next)::accum)

  def elemsExceptAsReverseExtras( nonExtra : Vector[Elem] )( nlist : List[Node] )( using pconfig : Parser.Config ) : List[Element.Extra] =
    _elemsExceptAsReverseExtras( nonExtra, Nil )( nlist )

  @tailrec
  private def _elemsExceptAsReverseExtras( nonExtra : Vector[Elem], accum : List[Element.Extra] )( nlist : List[Node] )( using pconfig : Parser.Config ) : List[Element.Extra] =
    nlist match
      case Nil => accum
      case next :: rest =>
        next match
          case elem : Elem =>
            val loc = nonExtra.indexOf(elem)
            loc match
              case -1 => _elemsExceptAsReverseExtras( nonExtra, Element.Extra(elem)::accum )( rest )
              case  n =>
                val excised = nonExtra.slice(0,n) ++ nonExtra.drop(n+1)
                _elemsExceptAsReverseExtras( excised, accum )( rest )
          case _ =>
            _elemsExceptAsReverseExtras( nonExtra, accum )( rest )

  // XXX: We're not using pconfig yet, but eventually, when we've implemented a lot of parsers,
  //      we'll try to fill-in the first part of the Element.Extras we create, and it will matter there
  def childElemsBeyondAsReverseExtras( expected : (Tuple2[String,Int]|String)* )( elem : Elem )( using pconfig : Parser.Config ) : List[Element.Extra]
    = elemsBeyondAsReverseExtras(expected*)( elem.child.toList )

  // XXX: We're not using pconfig yet, but eventually, when we've implemented a lot of parsers,
  //      we'll try to fill-in the first part of the Element.Extras we create, and it will matter there
  def elemsBeyondAsReverseExtras( expected : (Tuple2[String,Int]|String)* )( nlist : List[Node] )( using pconfig : Parser.Config ) : List[Element.Extra] =
    def tuplize( fullLabel : String ) : Tuple2[Option[Namespace],String] =
      val colonIndex = fullLabel.lastIndexOf(':')
      if colonIndex >= 0 then
        val prefix = fullLabel.substring(0,colonIndex)
        val namespace : Option[Namespace] = Namespace.byPrefix(prefix).orElse:
          throw new UnsupportedNamespace( s"Can't find namespace with prefix '${prefix}'." )
        ( namespace, fullLabel.substring(colonIndex + 1) )
      else
        ( None, fullLabel )

    val parsedExpected : Seq[Tuple2[Tuple2[Option[Namespace],String],Int]|Tuple2[Option[Namespace],String]]=
      expected map: xp =>
        xp match
          case ( fullLabel, n )   => Tuple2( tuplize(fullLabel), n )
          case fullLabel : String => tuplize(fullLabel)

    tuplizedElemsBeyondAsReverseExtras( parsedExpected* )(nlist)

  // XXX: We're not using pconfig yet, but eventually, when we've implemented a lot of parsers,
  //      we'll try to fill-in the first part of the Element.Extras we create, and it will matter there
  def tuplizedElemsBeyondAsReverseExtras( expected : (Tuple2[Tuple2[Option[Namespace],String],Int]|Tuple2[Option[Namespace],String])* )( nlist : List[Node] )( using pconfig : Parser.Config ) : List[Element.Extra] =
    val _expected =
      expected.map( (arg : Tuple2[Tuple2[Option[Namespace],String],Int] | Tuple2[Option[Namespace],String]) =>
          arg match
            case full @    ( _, _ : Int )                       => (full).asInstanceOf[Tuple2[Tuple2[Option[Namespace],String],Int]]                    // i'd hoped we could infer this type, but apparently not
            case partial @ ( _ : Option[Namespace], _ : String) => (partial -> Int.MaxValue).asInstanceOf[Tuple2[Tuple2[Option[Namespace],String],Int]] // i'd hoped we could infer this type, but apparently not
        )
        .toMap
    val expectedNamespaces = _expected.keySet.map( _(0) ).flatten
    _tuplizedElemsBeyondAsReverseExtras(Nil,expectedNamespaces,_expected)(nlist)

  @tailrec
  private def _tuplizedElemsBeyondAsReverseExtras(accum : List[Element.Extra], expectedNamespaces : Set[Namespace], expected : Map[Tuple2[Option[Namespace],String],Int] )( nlist : List[Node] )( using pconfig : Parser.Config ) : List[Element.Extra] =
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
              _tuplizedElemsBeyondAsReverseExtras(accum, expectedNamespaces, newExpected)(rest)
            case None =>
              val extra = Element.Extra(e)
              _tuplizedElemsBeyondAsReverseExtras(extra :: accum, expectedNamespaces, expected)( rest )
        case other =>
          _tuplizedElemsBeyondAsReverseExtras(accum, expectedNamespaces, expected)(rest) // we ignore/filter any non-Elems

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

  def elemsOfKinds( kinds : Element.Kinds, nodes : Seq[Node] ) : Seq[Elem] =
    nodes.collect { case elem : Elem if Element.Kind.forElem(elem).fold(false)( kind => Element.Kinds.contains(kinds, kind) ) => elem }
