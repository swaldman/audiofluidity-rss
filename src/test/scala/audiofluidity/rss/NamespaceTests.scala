package audiofluidity.rss

import utest.*
import scala.annotation.tailrec
import scala.xml.{Elem,NamespaceBinding,TopScope,XML}

object NamespaceTests extends TestSuite:

  // fromElemsRecursive and fromElemsDirect were rewritten to accumulate into a Set rather than
  // into a List deduplicated only by a trailing toSet, because the List form held one live
  // Namespace (plus an Option for its prefix, plus a cons cell) per binding per node. A heap
  // dump from a merge of a few hundred feeds held 6,460,835 live Namespace instances
  // representing 33 distinct values. The same rewrite replaced descendant_or_self, which
  // materializes an entire subtree as a List, with a lazy walk.
  //
  // Both changes were meant to be semantically invisible, so the pre-rewrite algorithm is
  // reproduced verbatim below and the current implementation is asserted to agree with it.
  // These are equivalence tests, not memory tests; peak-memory behaviour is too
  // environment-sensitive to assert here.

  @tailrec
  def oldNamespaces( accum : List[Namespace], binding : NamespaceBinding ) : List[Namespace] =
    if binding == TopScope || binding == null then accum
    else oldNamespaces( Namespace( binding.prefix, binding.uri ) :: accum, binding.parent )

  def oldNamespaces( accum : List[Namespace], bindingSeq : Seq[NamespaceBinding] ) : List[Namespace] =
    bindingSeq.foldLeft( accum )( ( soFar, next ) => oldNamespaces( soFar, next ) )

  def oldFromElemsRecursive( elems : Elem* ) : Set[Namespace] =
    elems.foldLeft( List.empty[Namespace] )( ( soFar, next ) => oldNamespaces( soFar, next.descendant_or_self.map( _.scope ) ) ).toSet

  def oldFromElemsDirect( elems : Elem* ) : Set[Namespace] =
    elems.foldLeft( List.empty[Namespace] )( ( soFar, next ) => oldNamespaces( soFar, next.map( _.scope ) ) ).toSet

  def prefixesOf( namespaces : Set[Namespace] ) : Set[Option[String]] = namespaces.map( _.prefix )

  /** Everything declared on the root, the ordinary RSS shape. Many nodes, few namespaces. */
  val flatFeed : Elem =
    val decls = ( 1 to 12 ).map( i => s"""xmlns:ns$i="http://example.com/ns/$i"""" ).mkString(" ")
    val items = ( 1 to 40 ).map( i => s"""<item><title>Item $i</title><ns3:extra>x$i</ns3:extra></item>""" ).mkString
    XML.loadString( s"""<rss version="2.0" $decls><channel><title>t</title>$items</channel></rss>""" )

  /**
    * Declarations on inner elements rather than the root, and non-Elem children (a comment,
    * a CDATA section) along the way, since the rewrite changed how the tree is traversed.
    * c is declared two levels below the item that declares b.
    */
  val nestedFeed : Elem =
    XML.loadString(
      """<rss xmlns:a="http://example.com/a"><channel><!-- a comment -->
        |<item xmlns:b="http://example.com/b"><title>t</title>
        |<b:x xmlns:c="http://example.com/c"><![CDATA[raw]]><c:y/></b:x></item>
        |<item xmlns:d="http://example.com/d"><d:z/></item>
        |</channel></rss>""".stripMargin
    )

  /** A default namespace, whose NamespaceBinding carries a null prefix. */
  val defaultNsFeed : Elem =
    XML.loadString("""<feed xmlns="http://www.w3.org/2005/Atom" xmlns:e="http://example.com/e"><entry><e:q/></entry></feed>""")

  val tests = Tests:

    test("recursiveMatchesPreRewriteAlgorithm"):
      Namespace.fromElemsRecursive( flatFeed )      ==> oldFromElemsRecursive( flatFeed )
      Namespace.fromElemsRecursive( nestedFeed )    ==> oldFromElemsRecursive( nestedFeed )
      Namespace.fromElemsRecursive( defaultNsFeed ) ==> oldFromElemsRecursive( defaultNsFeed )

    test("directMatchesPreRewriteAlgorithm"):
      Namespace.fromElemsDirect( flatFeed )      ==> oldFromElemsDirect( flatFeed )
      Namespace.fromElemsDirect( nestedFeed )    ==> oldFromElemsDirect( nestedFeed )
      Namespace.fromElemsDirect( defaultNsFeed ) ==> oldFromElemsDirect( defaultNsFeed )

    test("multipleRootsMatchPreRewriteAlgorithm"):
      Namespace.fromElemsRecursive( flatFeed, nestedFeed, defaultNsFeed ) ==>
        oldFromElemsRecursive( flatFeed, nestedFeed, defaultNsFeed )

    // The rest assert absolutely rather than against the control, so that the expected
    // behaviour is legible without reading the reproduced algorithm.

    test("recursiveFindsDeclarationsOnInnerElements"):
      prefixesOf( Namespace.fromElemsRecursive( nestedFeed ) ) ==> Set( Some("a"), Some("b"), Some("c"), Some("d") )

    test("directSeesOnlyTheRootsOwnScope"):
      prefixesOf( Namespace.fromElemsDirect( nestedFeed ) ) ==> Set( Some("a") )

    test("aDefaultNamespaceBecomesAnUnprefixedNamespace"):
      Namespace.fromElemsRecursive( defaultNsFeed ) ==>
        Set( Namespace( None, "http://www.w3.org/2005/Atom" ), Namespace( Some("e"), "http://example.com/e" ) )

    test("repeatedBindingsCollapseToDistinctNamespaces"):
      // flatFeed is ~120 nodes each inheriting the root's 12 bindings, so well over a
      // thousand binding encounters must reduce to twelve namespaces.
      val found = Namespace.fromElemsRecursive( flatFeed )
      found.size ==> 12
      prefixesOf( found ) ==> ( 1 to 12 ).map( i => Some(s"ns$i") ).toSet

    test("multipleRootsAreUnioned"):
      Namespace.fromElemsRecursive( nestedFeed, defaultNsFeed ) ==>
        ( Namespace.fromElemsRecursive( nestedFeed ) ++ Namespace.fromElemsRecursive( defaultNsFeed ) )
