package example

import scala.annotation.tailrec
import scala.language.postfixOps

object Interview extends FixtureAndSolution with App {

  /**
    * demonstrating fixtures and tree operations
    */
  override def main(args: Array[String]): Unit = {
    println(testGraph1.indexes(25).toList)

    println("-= graph1")
    printSearch(testGraph1)
    testGraph1 = testGraph1.addVal("4-3-1-1")
    println("-= graph1 after")
    printSearch(testGraph1)


    // we may test different migrations on these fixtures
    println("-= graph2")
    printSearch(testGraph2)
    println("-= graph3")
    printSearch(testGraph3)
    println("-= graph4")
    printSearch(testGraph4)

    // for instance:
    println("-= graph5 before")
    printSearch(testGraph5)
    testGraph5 = testGraph5.addVal("2-1")
    testGraph5 = testGraph5.addVal("2-2")
    println("-graph5 after")
    printSearch(testGraph5)
    testGraph5 = testGraph5.addVal("3-1-1")
    testGraph5 = testGraph5.addVal("3-1-2")
    println("-graph5 after")
    printSearch(testGraph5)
    testGraph5 = testGraph5.addVal("3-2-1")
    println("-graph5 after")
    printSearch(testGraph5)

    for (i <- 1 to 1000) testGraph5 = testGraph5.addVal(s"i${i.toString}")
    printSearch(testGraph5)
  }

  // debug node printing
  private def printSearch(graph: Tree[String]): Unit = {
    val srch = graph.breadthFirst()
    for (n <- srch) println(s"'${n._1.value}', level ${n._2}, parent '${n._3.value}'")

    val maxLevel = srch.maxBy(_._2)._2
    //    println(s"last level $maxLevel with indexes:")
    val max2 = (srch zipWithIndex).filter(_._1._2 == maxLevel)
    //    for (n <- max2) println(s"'${n._1._1.value}'", n._2)
    val (lastIndex, lastLevel) = (max2.last._2, max2.last._1._2)
    println(s"last index: $lastIndex, last level: $lastLevel")

  }

}

trait FixtureAndSolution {

  var testGraph2: Tree[String] = Tree[String](
    Node("1",
      Some(Node("2-1",
        Some(Node("3-1-1", None, None)),
        Some(Node("3-1-2", None, None))
      )),
      Some(Node("2-2",
        Some(Node("3-2-1", None, None)),
        None
      ))
    )
  )
  var testGraph3: Tree[String] = Tree[String](
    Node("1",
      Some(Node("2-1",
        Some(Node("3-1-1", None, None)),
        Some(Node("3-1-2", None, None))
      )),
      Some(Node("2-2", None, None)))
  )
  var testGraph4: Tree[String] = Tree[String](
    Node("1",
      Some(Node("2-1", None, None)),
      Some(Node("2-2", None, None)
      )
    )
  )
  var testGraph1: Tree[String] = Tree[String](
    Node("1",
      Some(Node("2-1",
        Some(Node("3-1-1", None, None)),
        Some(Node("3-1-2", None, None))
      )),
      Some(Node("2-2",
        Some(Node("3-2-1", None, None)),
        Some(Node("3-2-2", None, None))
      ))
    )
  )
  var testGraph5: Tree[String] = Tree[String](
    Node("1", None, None))


  case class Node[T](value: T, left: Option[Node[T]], right: Option[Node[T]])

  case class Tree[T](root: Node[T]) {

    type NodePlusLevel = (Node[T], Int)
    type NodePlusLevelPlusParent = (Node[T], Int, Node[T])


    /**
      *
      * @param value new value to add
      * @return a copy of a tree
      */
    def addVal(value: T): Tree[T] = Tree(nextCandidates(value))


    // a starter building a stream of indexes hierarchy
    def indexes(fromIdx: Int): Stream[Int] = idxs(fromIdx #:: Stream.empty, fromIdx)

    // parent indexes hierarchy in a stream
    def idxs(accum: Stream[Int], curr: Int): Stream[Int] = {
      // calculating corresponding parent index corresponding to a binary tree
      val nxt = math.floor((curr - 1) / 2).toInt
      if ((accum.isEmpty) || (curr == 0)) accum
      else accum.head #:: idxs(accum.tail append Stream(nxt), nxt)
    }

    /**
      * next candidate for adding a child node
      * according to breadth-first
      *
      * @return changed tree copying corresponding nodes
      */
    def nextCandidates(newVal: T): Node[T] = {
      // a lazy list of all the nodes breadth-first
      val srch: Stream[NodePlusLevelPlusParent] = breadthFirst(this.root)
      // last nesting level
      val maxLevel = srch.maxBy(_._2)._2
      val max2 = (srch zipWithIndex) filter (_._1._2 == maxLevel)
      val lastIndex = max2.last._2
      val candidateIndex = math.floor(lastIndex / 2).toInt
      // calculated (from bottom to the top) indexes of nodes that are to change
      val changedIndexes = indexes(candidateIndex)
      // a nre node to add
      val newNode = Node(newVal, None, None)
      // changed tree copying corresponding nodes
      newHierarchy(srch, changedIndexes, newNode)
    }


    /**
      * a lazy list of graph nodes arranged breadth-first
      *
      * @param startRoot a starting node
      * @return a nodes stream
      */
    def breadthFirst(startRoot: Node[T] = this.root): Stream[(Node[T], Int, Node[T])] = {
      /**
        * inner breadth first search applying a function to add nodes to a lazy list
        */
      def bfs[T](s: Stream[(T, Int, T)], f: (T, Int, T) => Stream[(T, Int, T)]): Stream[(T, Int, T)] = {
        if (s.isEmpty) s
        else s.head #:: bfs(s.tail append f(s.head._1, s.head._2, s.head._3), f)
      }

      bfs[Node[T]](
        Stream[(Node[T], Int, Node[T])]((startRoot, 0, startRoot)),
        (node, level, parent) =>
          (node.left, node.right) match {
            case (Some(_), Some(_)) => (node.left.get, level + 1, node) #:: (node.right.get, level + 1, node) #:: Stream.empty
            case (Some(_), None) => Stream[(Node[T], Int, Node[T])]((node.left.get, level + 1, node))
            case (None, None) => Stream.empty
          }
      )
    }


    /**
      * a changed tree copying corresponding nodes
      * recursive function starting with a node to add
      *
      * @param nodes       a whole tree nodes lazy list ranged breadth-first
      * @param indexes     a hierarchy of indexes of nodes that are to change
      * @param changedNode a new node to start with
      * @param prevIndex   an index of a node changing its parent
      * @param adding      first applying flag
      * @return
      */
    @tailrec
    private def newHierarchy(nodes: Stream[NodePlusLevelPlusParent],
                             indexes: Stream[Int],
                             changedNode: Node[T],
                             prevIndex: Int = -1,
                             adding: Boolean = true): Node[T] = {

      if (indexes.isEmpty) changedNode else {
        val lastIndex = indexes.head
        val parentNode = nodes(lastIndex)._1
        val newNode = if (adding) {
          if (parentNode.left.isEmpty)
            parentNode.copy(left = Some(changedNode))
          else
            parentNode.copy(right = Some(changedNode))
        } else {
          if (prevIndex % 2 == 0)
            parentNode.copy(right = Some(changedNode))
          else
            parentNode.copy(left = Some(changedNode))
        }
        newHierarchy(nodes, indexes.tail, newNode, lastIndex, adding = false)
      }
    }

  }


}
