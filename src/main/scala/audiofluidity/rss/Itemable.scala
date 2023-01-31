package audiofluidity.rss

import scala.collection.*
import scala.xml.Elem

object Itemable:
  given ItemsAreItemable : Itemable[Element.Item] with
    extension( item : Element.Item )
      def toItemWithDecorations : (Element.Item, immutable.Seq[Elem]) = (item, Nil)

/**
 * Decorations are arbitrary XML elements that should be added as _children_
 * of the standard RSS item.
 */
trait Itemable[T]:
  extension( t : T )
    def toItemWithDecorations : (Element.Item, immutable.Seq[Elem])
