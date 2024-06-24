package audiofluidity.rss

import audiofluidity.rss.util.defaultNamespaceUri

import scala.xml.{NodeSeq,Elem}

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


