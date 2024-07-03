package audiofluidity.rss

import scala.annotation.tailrec
import scala.collection.mutable
import scala.xml.MetaData

object Synthetic:

  def forwardExtrasExceptElements( elements : Vector[Element[?]] )( reverseExtras : List[Element.Extra] ) : List[Element.Extra] =
    _forwardExtrasExceptElements( elements, Nil )( reverseExtras )

  @tailrec
  private def _forwardExtrasExceptElements( elements : Vector[Element[?]], accum : List[Element.Extra] )( reverseExtras : List[Element.Extra] ) : List[Element.Extra] =
    reverseExtras match
      case Nil => accum
      case (extra @ Element.Extra( Some( element ), _ )) :: rest =>
        val elementsPos = elements.indexOf( element )
        if elementsPos >= 0 then
          val excised = elements.take(elementsPos) ++ elements.drop(elementsPos+1)
          _forwardExtrasExceptElements(excised, accum)( rest )
        else
          _forwardExtrasExceptElements(elements, extra :: accum)( rest )
      case next :: rest =>
          _forwardExtrasExceptElements(elements, next :: accum)( rest )

  def fromGenericSynthetic( synthetic : Element.Iffy.Synthetic ) : ( Seq[String], Option[Synthetic] ) =
    val warnings = Vector.newBuilder[String]
    synthetic.`type` match // .getOrElse(throw new UnparsableSynthetic( Seq("Missing type.") )()).value
      case None => ( Seq("Missing type."), None )
      case Some( typeElem ) =>
        Element.Iffy.Synthetic.KnownType.lenientParse( typeElem.value ) match
          case Some( knownType ) =>
            knownType match
              case Element.Iffy.Synthetic.KnownType.ItemUpdateFeed => fromItemUpdateFeedSynthetic( warnings )( typeElem, synthetic )
              case Element.Iffy.Synthetic.KnownType.UpdateAnnouncement => fromUpdateAnnouncementSynthetic( warnings )( typeElem, synthetic )
              case Element.Iffy.Synthetic.KnownType.UpdateCumulation => fromUpdateCumulationSynthetic( warnings )( typeElem, synthetic )
          case None => ( Seq("Missing type."), None )

  def fromItemUpdateFeedSynthetic( warnings : mutable.ReusableBuilder[String, Vector[String]] )( typeElement : Element.Iffy.Type, synthetic : Element.Iffy.Synthetic ) : ( Seq[String], Option[Synthetic] ) =
    val used     = Vector.newBuilder[Element[?]]
    val mbProvenance = synthetic.extraElements.collect{ case p : Element.Iffy.Provenance => p }.headOption
    synthetic.extraElements.collect{ case l : Element.Atom.Link => l }.headOption match
      case None => ( Seq("atom:link element required, not found."), None )
      case Some( itemLink ) =>
        used ++= mbProvenance
        used += itemLink
        val forwardExtras = forwardExtrasExceptElements(used.result)( synthetic.reverseExtras )
        val extraAttributs = synthetic.extraAttributes
        ( warnings.result, Some( ItemUpdateFeed( typeElement, forwardExtras, extraAttributs, mbProvenance, itemLink ) ) )

  def fromUpdateAnnouncementSynthetic( warnings : mutable.ReusableBuilder[String, Vector[String]] )( typeElement : Element.Iffy.Type, synthetic : Element.Iffy.Synthetic ) : ( Seq[String], Option[Synthetic] ) =
    val used = Vector.newBuilder[Element[?]]
    val mbProvenance = synthetic.extraElements.collect{ case p : Element.Iffy.Provenance => p }.headOption
    synthetic.extraElements.collect{ case u : Element.Iffy.Update => u }.headOption match
        case None => ( Seq("iffy:update element required, not found."), None )
        case Some( update ) =>
          used ++= mbProvenance
          used += update
          val forwardExtras = forwardExtrasExceptElements(used.result)( synthetic.reverseExtras )
          val extraAttributs = synthetic.extraAttributes
          ( warnings.result, Some( UpdateAnnouncement( typeElement, forwardExtras, extraAttributs, mbProvenance, update ) ) )

  def fromUpdateCumulationSynthetic( warnings : mutable.ReusableBuilder[String, Vector[String]] )( typeElement : Element.Iffy.Type, synthetic : Element.Iffy.Synthetic ) :( Seq[String], Option[Synthetic] ) =
    val used = Vector.newBuilder[Element[?]]
    val mbProvenance = synthetic.extraElements.collect{ case p : Element.Iffy.Provenance => p }.headOption
    val updateHistories = synthetic.extraElements.collect{ case uh : Element.Iffy.UpdateHistory => uh }
    if updateHistories.isEmpty then
      ( Seq("No iffy:update-history elements found in iffy:synthetic of type UpdateCumulation"), None )
    else
      used ++= mbProvenance
      used ++= updateHistories
      val forwardExtras = forwardExtrasExceptElements(used.result)( synthetic.reverseExtras )
      val extraAttributs = synthetic.extraAttributes
      ( warnings.result, Some( UpdateCumulation( typeElement, forwardExtras, extraAttributs, mbProvenance, updateHistories ) ) )

  case class ItemUpdateFeed( `type` : Element.Iffy.Type, forwardExtras : List[Element.Extra], extraAttributes : MetaData, provenance : Option[Element.Iffy.Provenance], itemLink : Element.Atom.Link ) extends Synthetic.Derived:
    def knownNontypeElements : List[Element[?]] = provenance.toList ::: itemLink :: Nil

  case class UpdateAnnouncement( `type` : Element.Iffy.Type, forwardExtras : List[Element.Extra], extraAttributes : MetaData, provenance : Option[Element.Iffy.Provenance], update : Element.Iffy.Update ) extends Synthetic.Derived:
    def knownNontypeElements : List[Element[?]] = provenance.toList ::: update :: Nil

  case class UpdateCumulation( `type` : Element.Iffy.Type, forwardExtras : List[Element.Extra], extraAttributes : MetaData, provenance : Option[Element.Iffy.Provenance], updateHistories : Seq[Element.Iffy.UpdateHistory] ) extends Synthetic.Derived:
    def knownNontypeElements : List[Element[?]] = provenance.toList ::: updateHistories.toList

  trait Derived extends Synthetic:
    def provenance : Option[Element.Iffy.Provenance]

trait Synthetic:
  def `type`          : Element.Iffy.Type
  def forwardExtras   : List[Element.Extra]
  def extraAttributes : MetaData

  def knownNontypeElements : List[Element[?]]

  def toElement       : Element.Iffy.Synthetic =
    val reverseKnownNontypeExtras = knownNontypeElements.map( element => Element.Extra( element ) ).reverse
    Element.Iffy.Synthetic(`type` = Some(`type`), namespaces = Nil, reverseExtras = forwardExtras.reverse ::: reverseKnownNontypeExtras, extraAttributes = extraAttributes, asLastParsed = None )



