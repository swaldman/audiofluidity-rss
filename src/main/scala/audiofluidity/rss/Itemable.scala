package audiofluidity.rss

import scala.collection.*
import scala.xml.Elem

object Itemable:
  given ItemsAreItemable : Itemable[Element.Item] with
    extension( item : Element.Item )
      def toItem : Element.Item = item

trait Itemable[T]:
  extension( t : T )
    def toItem : Element.Item
