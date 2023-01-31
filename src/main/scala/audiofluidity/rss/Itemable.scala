package audiofluidity.rss

import scala.collection.*
import scala.xml.Elem

object Itemable:
  given ItemsAreItemable : Itemable[Element.Item] with
    extension( item : Element.Item )
      def toItem : Element.Item = item

/**
 * Decorations are arbitrary XML elements that should be added as _children_
 * of the standard RSS item.
 */
trait Itemable[T]:
  extension( t : T )
    def toItem : Element.Item
    def itemDecorations : immutable.Seq[Elem] = Nil
