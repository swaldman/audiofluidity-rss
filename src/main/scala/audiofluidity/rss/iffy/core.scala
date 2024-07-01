package audiofluidity.rss.iffy

import scala.collection.{immutable,mutable}

import audiofluidity.rss.*
import java.time.ZonedDateTime

def cumulateUpdateAnnouncements( items : Seq[Element.Item] ) : ( Seq[String], Element.Iffy.Synthetic ) = // returns an UpdateCumulation
  val warnings = Vector.newBuilder[String]
  val updateAnnouncements =
    items.flatMap: item =>
      val synthetics =
        item.reverseExtras.collect { case Element.Extra( Some( synthetic : Element.Iffy.Synthetic ), _ ) => synthetic }
      synthetics.size match
        case 1 =>
          synthetics.head.`type` match
            case Some( tpe ) if tpe.value == Element.Iffy.Synthetic.KnownType.UpdateAnnouncement.toString => Seq( synthetics.head )
            case _ => Nil
        case n =>
          warnings += s"Found more than one ($n) synthetic element in item, can't interpret, skipping. " + item
          Nil
  val ( ws, synth ) = _cumulateUpdateAnnouncements(updateAnnouncements)
  warnings ++= ws
  ( warnings.result, synth )

// expects input already to be filtered on synthetic type UpdateAnnouncement
private def _cumulateUpdateAnnouncements( updateAnnouncements : Seq[Element.Iffy.Synthetic] ) : ( Seq[String], Element.Iffy.Synthetic ) = // returns an UpdateCumulation
  val warnings = Vector.newBuilder[String]
  def updateElementToTup3( update : Element.Iffy.Update ) : Option[(String, Element.Iffy.Initial, Element.Iffy.Update)] = // ( guid, initial, initial-less update )
    val mbInitial = update.initial
    val mbGuid = mbInitial.flatMap( initial => initial.guid ).map( _.value )
    val initialFreeUpdate = update.copy( initial = None )
    for
      guid <- mbGuid
      initial <- mbInitial
    yield
      ( guid, initial, initialFreeUpdate )
  val updateElements = // XXX: we (silently for now!) skip unexpected/unparseable children!
    updateAnnouncements.flatMap: ua =>
      ua.reverseExtras.collect { case Element.Extra( Some( update : Element.Iffy.Update ), _ ) => update }
  val tups3 =
    updateElements.flatMap: ue =>
      val out = updateElementToTup3(ue)
      if out.isEmpty then
        warnings += "Incomplete update element. Skipping: " + ue
      out
  given Ordering[Element.Iffy.Update] =
    Ordering.by[Element.Iffy.Update,ZonedDateTime]( _.updated.zdt ).reverse
  val guidToInitialAndUpdates : mutable.Map[String,(Element.Iffy.Initial,mutable.SortedSet[Element.Iffy.Update])] = // mutable but local, single threaded
    val tmp = mutable.Map.empty[String,(Element.Iffy.Initial,mutable.SortedSet[Element.Iffy.Update])]
    tups3.foreach: ( guid, initial, update ) =>
      val tup2 = tmp.getOrElseUpdate( guid, (initial, mutable.SortedSet.empty ) )
      tup2(1) += update
    tmp
  def updateHistory( guid : String ) : Element.Iffy.UpdateHistory =
    val ( initial, updates ) = guidToInitialAndUpdates(guid) // we are iterating keys, so we can assert the value exists
    Element.Iffy.UpdateHistory( updates.toSeq, Some(initial) )
  given Ordering[Element.Iffy.UpdateHistory] =
    Ordering.by[Element.Iffy.UpdateHistory,ZonedDateTime]( _.updates.head.updated.zdt ).reverse // XXX: We should be more robust to illegal updateless update histories
  val updateHistories = immutable.SortedSet.from( guidToInitialAndUpdates.keySet.map( updateHistory ) )
  ( warnings.result, Element.Iffy.Synthetic(Some(Element.Iffy.Type(Element.Iffy.Synthetic.KnownType.UpdateCumulation.toString))).withExtras(updateHistories) )




