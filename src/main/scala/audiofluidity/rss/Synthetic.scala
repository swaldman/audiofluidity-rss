package audiofluidity.rss

import scala.xml.MetaData

object Synthetic:

  def forwardExtrasExceptElements( elements : Vector[Element[?]] )( reverseExtras : List[Element.Extra] ) : List[Element.Extra] =
    _forwardExtrasExceptElements( elements, Nil )( reverseExtras )

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

  def fromGenericSynthetic( synthetic : Element.Iffy.Synthetic ) : Synthetic =
    val `type` =
      val typeValue = synthetic.`type`.getOrElse(throw new UnparsableSynthetic( Seq("Missing type.") )()).value
      Element.Iffy.Synthetic.KnownType.lenientParse( typeValue ) match
        case Some( knownType ) => knownType
        case None              => throw new UnparsableSynthetic( Nil, Some(typeValue) )()
    `type` match
      case Element.Iffy.Synthetic.KnownType.ItemUpdateFeed => fromItemUpdateFeedSynthetic( synthetic )
      case Element.Iffy.Synthetic.KnownType.UpdateAnnouncement => fromUpdateAnnouncementSynthetic( synthetic )
      case Element.Iffy.Synthetic.KnownType.UpdateCumulation => fromUpdateCumulationSynthetic( synthetic )

  def fromItemUpdateFeedSynthetic( synthetic : Element.Iffy.Synthetic ) : Synthetic =
    //val warnings = Vector.newBuilder[String]
    val used     = Vector.newBuilder[Element[?]]
    val mbProvenance = synthetic.extraElements.collect{ case p : Element.Iffy.Provenance => p }.headOption
    val itemLink =
      synthetic.extraElements.collect{ case l : Element.Atom.Link => l }.headOption.getOrElse:
        throw new UnparsableSynthetic( Seq("atom:link element required, not found.") )()
    used ++= mbProvenance
    used += itemLink
    val parseWarnings = Nil
    val forwardExtras = forwardExtrasExceptElements(used.result)( synthetic.reverseExtras )
    val extraAttributs = synthetic.extraAttributes
    ItemUpdateFeed( parseWarnings, forwardExtras, extraAttributs, mbProvenance, itemLink )

  def fromUpdateAnnouncementSynthetic( synthetic : Element.Iffy.Synthetic ) : Synthetic =
    val used = Vector.newBuilder[Element[?]]
    val mbProvenance = synthetic.extraElements.collect{ case p : Element.Iffy.Provenance => p }.headOption
    val update =
      synthetic.extraElements.collect{ case u : Element.Iffy.Update => u }.headOption.getOrElse:
        throw new UnparsableSynthetic( Seq("iffy:update element required, not found.") )()
    used ++= mbProvenance
    used += update
    val parseWarnings = Nil
    val forwardExtras = forwardExtrasExceptElements(used.result)( synthetic.reverseExtras )
    val extraAttributs = synthetic.extraAttributes
    UpdateAnnouncement( parseWarnings, forwardExtras, extraAttributs, mbProvenance, update )

  def fromUpdateCumulationSynthetic( synthetic : Element.Iffy.Synthetic ) : Synthetic =
    val used = Vector.newBuilder[Element[?]]
    val mbProvenance = synthetic.extraElements.collect{ case p : Element.Iffy.Provenance => p }.headOption
    val updateHistories = synthetic.extraElements.collect{ case uh : Element.Iffy.UpdateHistory => uh } // XXX: warn if empty!
    used ++= mbProvenance
    used ++= updateHistories
    val parseWarnings = Nil
    val forwardExtras = forwardExtrasExceptElements(used.result)( synthetic.reverseExtras )
    val extraAttributs = synthetic.extraAttributes
    UpdateCumulation( parseWarnings, forwardExtras, extraAttributs, mbProvenance, updateHistories )


  case class ItemUpdateFeed( parseWarnings : Seq[String], forwardExtras : List[Element.Extra], extraAttributes : MetaData, provenance : Option[Element.Iffy.Provenance], itemLink : Element.Atom.Link ) extends Synthetic.Derived
  case class UpdateAnnouncement( parseWarnings : Seq[String], forwardExtras : List[Element.Extra], extraAttributes : MetaData, provenance : Option[Element.Iffy.Provenance], update : Element.Iffy.Update ) extends Synthetic.Derived
  case class UpdateCumulation( parseWarnings : Seq[String], forwardExtras : List[Element.Extra], extraAttributes : MetaData, provenance : Option[Element.Iffy.Provenance], updateHistories : Seq[Element.Iffy.UpdateHistory] ) extends Synthetic.Derived

  trait Derived extends Synthetic:
    def provenance : Option[Element.Iffy.Provenance]

trait Synthetic:
  def parseWarnings   : Seq[String]
  def forwardExtras   : List[Element.Extra]
  def extraAttributes : MetaData



