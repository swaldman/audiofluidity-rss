package audiofluidity.rss

import scala.annotation.tailrec
import scala.collection.{immutable,mutable}
import scala.xml.{MetaData,Null}

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

  private def fromItemUpdateFeedSynthetic( warnings : mutable.ReusableBuilder[String, Vector[String]] )( typeElement : Element.Iffy.Type, synthetic : Element.Iffy.Synthetic ) : ( Seq[String], Option[Synthetic] ) =
    val used     = Vector.newBuilder[Element[?]]
    val mbProvenance = synthetic.extraElements.collect{ case p : Element.Iffy.Provenance => p }.headOption
    synthetic.extraElements.collect{ case l : Element.Atom.Link => l }.headOption match
      case None => ( Seq("atom:link element required, not found."), None )
      case Some( itemLink ) =>
        used ++= mbProvenance
        used += itemLink
        val forwardExtras = forwardExtrasExceptElements(used.result)( synthetic.reverseExtras )
        val extraAttributs = synthetic.extraAttributes
        ( warnings.result, Some( ItemUpdateFeed( itemLink, mbProvenance, forwardExtras, extraAttributs, typeElement ) ) )

  private def fromUpdateAnnouncementSynthetic( warnings : mutable.ReusableBuilder[String, Vector[String]] )( typeElement : Element.Iffy.Type, synthetic : Element.Iffy.Synthetic ) : ( Seq[String], Option[Synthetic] ) =
    val used = Vector.newBuilder[Element[?]]
    val mbProvenance = synthetic.extraElements.collect{ case p : Element.Iffy.Provenance => p }.headOption
    synthetic.extraElements.collect{ case u : Element.Iffy.Update => u }.headOption match
        case None => ( Seq("iffy:update element required, not found."), None )
        case Some( update ) =>
          used ++= mbProvenance
          used += update
          val forwardExtras = forwardExtrasExceptElements(used.result)( synthetic.reverseExtras )
          val extraAttributs = synthetic.extraAttributes
          ( warnings.result, Some( UpdateAnnouncement( update, mbProvenance, forwardExtras, extraAttributs, typeElement ) ) )

  private def fromUpdateCumulationSynthetic( warnings : mutable.ReusableBuilder[String, Vector[String]] )( typeElement : Element.Iffy.Type, synthetic : Element.Iffy.Synthetic ) :( Seq[String], Option[Synthetic] ) =
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
      ( warnings.result, Some( UpdateCumulation( updateHistories, mbProvenance, forwardExtras, extraAttributs, typeElement ) ) )

  object ItemUpdateFeed:
    val DefaultTypeElement = Element.Iffy.Type(Element.Iffy.Synthetic.KnownType.ItemUpdateFeed.toString)
  case class ItemUpdateFeed(
    itemLink        : Element.Atom.Link,
    provenance      : Option[Element.Iffy.Provenance] = None,
    forwardExtras   : List[Element.Extra]             = Nil,
    extraAttributes : MetaData                        = Null,
    `type`          : Element.Iffy.Type               = ItemUpdateFeed.DefaultTypeElement
  ) extends Synthetic.Derived:
    def knownNontypeElements : List[Element[?]] = provenance.toList ::: itemLink :: Nil

  object UpdateAnnouncement:
    val DefaultTypeElement = Element.Iffy.Type(Element.Iffy.Synthetic.KnownType.UpdateAnnouncement.toString)
  case class UpdateAnnouncement(
    update          : Element.Iffy.Update,
    provenance      : Option[Element.Iffy.Provenance] = None,
    forwardExtras   : List[Element.Extra]             = Nil,
    extraAttributes : MetaData                        = Null,
    `type`          : Element.Iffy.Type               = UpdateAnnouncement.DefaultTypeElement
  ) extends Synthetic.Derived:
    def knownNontypeElements : List[Element[?]] = provenance.toList ::: update :: Nil

  object UpdateCumulation:
    val DefaultTypeElement = Element.Iffy.Type(Element.Iffy.Synthetic.KnownType.UpdateCumulation.toString)
    def computeProvenance( destinationFeedUrl : String, sourceFeedUrl : String, updateAnnouncements : Seq[UpdateAnnouncement] ) : Option[Element.Iffy.Provenance] =
      val sourceLink = Element.Atom.Link(href = sourceFeedUrl, rel = Some(Element.Atom.LinkRelation.via) )
      val simpleSourceProvenance = Element.Iffy.Provenance(Seq(sourceLink))
      def isSimpleSourceLink( item : Element.Atom.Link | Element.Iffy.Provenance ) : Boolean =
        def sameEnoughLink( link : Element.Atom.Link ) : Boolean = link.href == sourceLink.href && link.rel == sourceLink.rel
        item match
          case link : Element.Atom.Link             => sameEnoughLink(link)
          case provenance : Element.Iffy.Provenance => isSimpleSourceLink(provenance)
      val rawAnnouncementProvenances = updateAnnouncements.map( _.provenance ).flatten
      ( destinationFeedUrl == sourceFeedUrl, rawAnnouncementProvenances.isEmpty ) match
        case ( true, true )  => None
        case ( false, true ) => Some(simpleSourceProvenance)
        case ( _, false )    => Some(Element.Iffy.Provenance(simpleSourceProvenance +: rawAnnouncementProvenances.filterNot( isSimpleSourceLink ), shape = Some(Element.Iffy.Provenance.Shape.merge)))
    def cumulate( announcements : Seq[UpdateAnnouncement], provenance : Option[Element.Iffy.Provenance] = None ) : Either[CannotCumulateUpdates,UpdateCumulation] =
      import Element.Iffy.{Initial,Uid,Update,UpdateHistory}
      def triple( update : Element.Iffy.Update ) : Tuple3[String,Initial,Update] =
        val initial = update.initial.getOrElse( throw new CannotCumulateUpdates("An update is missing its required initial element.") )
        val uid = initial.guid.getOrElse( throw new CannotCumulateUpdates("An update's initial element is missing its required uid.") ).value
        ( uid, initial, update )
      given Ordering[Update] = Ordering.by( (update:Update) => (update.updated.zdt, update.toString) ).reverse
      try
        val ( initialsByUid, updatesByUid ) =
          announcements.map(ua=>triple(ua.update)).foldLeft( Tuple2(Map.empty[String,Initial],Map.empty[String,immutable.SortedSet[Update]]) ): ( accum, next ) =>
            val (ibu, ubu) = accum
            val (uid, initial, update) = next
            val lastUpdateSortedSet = ubu.get(uid).getOrElse(immutable.SortedSet.empty[Update])
            (ibu.updated(uid, initial), ubu.updated(uid, (lastUpdateSortedSet + update)))
        val updateHistories =
          updatesByUid.map { case (uid, updatesSortedSet) =>
            val updates = updatesSortedSet.toSeq
            val initial = initialsByUid(uid) // should never fail, we got complete triples
            UpdateHistory(updates,Some(initial))
          }
        Right(UpdateCumulation(updateHistories.toSeq,provenance))
      catch
        case ccu : CannotCumulateUpdates => Left(ccu)
  case class UpdateCumulation(
    updateHistories : Seq[Element.Iffy.UpdateHistory],
    provenance      : Option[Element.Iffy.Provenance] = None,
    forwardExtras   : List[Element.Extra]             = Nil,
    extraAttributes : MetaData                        = Null,
    `type`          : Element.Iffy.Type               = UpdateCumulation.DefaultTypeElement
  ) extends Synthetic.Derived:
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



