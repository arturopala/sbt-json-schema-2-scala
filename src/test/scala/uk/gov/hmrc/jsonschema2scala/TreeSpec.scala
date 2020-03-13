package uk.gov.hmrc.jsonschema2scala

import org.scalatest.{Matchers, WordSpec}
import uk.gov.hmrc.jsonschema2scala.utils.Tree

class TreeSpec extends WordSpec with Matchers {

  def showAsArrays(tree: Tree[Int]): String = tree.mkString(_.toString, ",", "\n", "[", "]")
  def showAsGraph(tree: Tree[Int]): String = tree.mkString(_.toString, " > ", "\n", "", "")

  "Tree" should {
    "create an empty Tree" in {
      val tree: Tree[Int] = Tree.empty
      tree.size shouldBe 0
      tree.numberOfBranches shouldBe 0
      tree.countBranches(_.nonEmpty) shouldBe 0
      tree.countBranches(_.isEmpty) shouldBe 0
      showAsArrays(tree) shouldBe ""
    }

    "create a single node Tree" in {
      val tree1 = Tree(0)
      tree1 shouldBe Tree.Node(0, Nil)
      tree1.size shouldBe 1
      tree1.numberOfBranches shouldBe 1
      tree1.countBranches(_.nonEmpty) shouldBe 1
      tree1.countBranches(_.isEmpty) shouldBe 1
      showAsArrays(tree1) shouldBe "[0]"
    }

    "create a double node Tree" in {
      val tree1 = Tree(0, Tree(1))
      tree1.size shouldBe 2
      tree1.numberOfBranches shouldBe 1
      showAsArrays(tree1) shouldBe "[0,1]"
    }

    "create a three nodes Tree" in {
      val tree1 = Tree(0, Tree(1, Tree(2)))
      tree1.size shouldBe 3
      tree1.numberOfBranches shouldBe 1
      showAsArrays(tree1) shouldBe "[0,1,2]"

      val tree2 = Tree(0, Tree(10), Tree(11))
      tree2.size shouldBe 3
      tree2.numberOfBranches shouldBe 2
      showAsArrays(tree2) shouldBe """[0,10]
                                     |[0,11]""".stripMargin
    }

    "create a four nodes Tree" in {
      val tree1 = Tree(0, Tree(1, Tree(2, Tree(3))))
      tree1.size shouldBe 4
      tree1.numberOfBranches shouldBe 1
      showAsArrays(tree1) shouldBe "[0,1,2,3]"

      val tree2 = Tree(0, Tree(1, Tree(20), Tree(21)))
      tree2.size shouldBe 4
      tree2.numberOfBranches shouldBe 2
      showAsArrays(tree2) shouldBe """[0,1,20]
                                     |[0,1,21]""".stripMargin

      val tree3 = Tree(0, Tree(10), Tree(11), Tree(12))
      tree3.size shouldBe 4
      tree3.numberOfBranches shouldBe 3
      showAsArrays(tree3) shouldBe """[0,10]
                                     |[0,11]
                                     |[0,12]""".stripMargin
    }

    "create a multi-branch Tree" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree.size shouldBe 9
      tree.numberOfBranches shouldBe 3
      val s = showAsArrays(tree)
      println(s)
      s shouldBe """[0,11,20,30]
                   |[0,12,21,31]
                   |[0,12,22,32]""".stripMargin
    }

    "check if the path exists" in {
      val tree = Tree.empty
      tree.contains(List(0, 1)) shouldBe false

      val tree1 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree1.contains(List(0, 11)) shouldBe true
      tree1.contains(List(0, 11, 20)) shouldBe true
      tree1.contains(List(0, 11, 30)) shouldBe false
    }

    "insert new node to an empty Tree" in {
      val tree: Tree[Int] = Tree.empty
      val tree2 = tree.insert(0)
      tree2 shouldBe Tree(0)
      tree2.size shouldBe 1
      tree2.numberOfBranches shouldBe 1
    }

    "insert new node to a single node Tree" in {
      val tree = Tree(0)
      val tree2 = tree.insert(1)
      tree2 shouldBe Tree(0, Tree(1))
      tree2.size shouldBe 2
      tree2.numberOfBranches shouldBe 1
    }

    "insert new branch to an empty Tree" in {
      val tree: Tree[Int] = Tree.empty
      val tree2 = tree.insert(List(0, 1, 2, 3))
      tree2 shouldBe Tree(0, Tree(1, Tree(2, Tree(3))))
      tree2.size shouldBe 4
      tree2.numberOfBranches shouldBe 1
    }

    "insert new branch to a single node Tree" in {
      val tree = Tree(0)
      tree.insert(List(0, 1, 2, 3)) shouldBe Tree(0, Tree(1, Tree(2, Tree(3))))
    }

    "insert new branch to a multi-branch Tree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3))))
      tree.size shouldBe 4
      tree.numberOfBranches shouldBe 1

      val tree2 = tree.insert(List(0, 1, 22, 33))
      tree2 shouldBe Tree(0, Tree(1, Tree(22, Tree(33)), Tree(2, Tree(3))))
      tree2.size shouldBe 6
      tree2.numberOfBranches shouldBe 2

      val tree3 = tree
        .insert(List(0, 1, 22, 33))
        .insert(List(0, 11, 12, 13))
      tree3 shouldBe Tree(0, Tree(11, Tree(12, Tree(13))), Tree(1, Tree(22, Tree(33)), Tree(2, Tree(3))))
      tree3.size shouldBe 9
      tree3.numberOfBranches shouldBe 3
    }

    "insert existing node to a multi-branch Tree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3))))
      tree.insert(List(0)) shouldBe tree
    }

    "insert existing branch to a multi-branch Tree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3))))
      tree.insert(List(0, 1, 2)) shouldBe tree
    }

    "try insert non-matching branch to a multi-branch Tree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3))))
      tree.insert(List(7, 0)) shouldBe tree
    }

    "select an existing subtree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3), Tree(4), Tree(5))))
      tree.select(List(0, 1)) shouldBe Tree(1, Tree(2, Tree(3), Tree(4), Tree(5)))
    }

    "try selecting non-existent subtree" in {
      val tree = Tree(0, Tree(1, Tree(2, Tree(3), Tree(4), Tree(5))))
      tree.select(List(1, 2)) shouldBe Tree.empty
    }

    "list all nodes" in {
      val tree = Tree.empty
      tree.nodes shouldBe Nil
      val tree1 = Tree(0)
      tree1.nodes shouldBe List(0)
      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.nodes shouldBe List(0, 11, 20, 30, 12, 21, 31, 22, 32)
    }

    "list all nodes using tail safe method" in {
      val tree = Tree.empty
      tree.nodesTS shouldBe Nil
      val tree1 = Tree(0)
      tree1.nodesTS shouldBe List(0)
      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.nodesTS shouldBe List(0, 11, 20, 30, 12, 21, 31, 22, 32)
    }

    "stream all nodes" in {
      val tree = Tree.empty
      tree.nodeStream.toList shouldBe Nil
      val tree1 = Tree(0)
      tree1.nodeStream.toList shouldBe List(0)
      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.nodeStream.toList shouldBe List(0, 11, 20, 30, 12, 21, 31, 22, 32)
    }

    "stream filtered nodes" in {
      val tree1 = Tree(0)
      tree1.nodeStream(_ > 15).toList shouldBe Nil
      val tree2 = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree2.nodeStream(_ > 15).toList shouldBe List(20, 30, 21, 31, 22, 32)
    }

    "list all branches" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      val expected = """0 > 11 > 20 > 30
                       |0 > 12 > 21 > 31
                       |0 > 12 > 22 > 32""".stripMargin

      val branches: List[List[Int]] = tree.branches

      val graph = branches.map(_.mkString(" > ")).mkString("\n")
      graph should be(expected)
      graph shouldBe showAsGraph(tree)
      branches.forall(tree.contains) shouldBe true
      branches.reverse.foldLeft[Tree[Int]](Tree.empty)(_.insert(_)) shouldBe tree
    }

    "list all branches using tail safe method" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      val expected = """0 > 11 > 20 > 30
                       |0 > 12 > 21 > 31
                       |0 > 12 > 22 > 32""".stripMargin

      val branches: List[List[Int]] = tree.branchesTS

      val graph = branches.map(_.mkString(" > ")).mkString("\n")
      graph should be(expected)
      graph shouldBe showAsGraph(tree)
      branches.forall(tree.contains) shouldBe true
      branches.reverse.foldLeft[Tree[Int]](Tree.empty)(_.insert(_)) shouldBe tree
    }

    "stream all branches" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      val graph = tree.branchStream.map(_.mkString(" > ")).mkString("\n")
      graph shouldBe """0 > 11 > 20 > 30
                       |0 > 12 > 21 > 31
                       |0 > 12 > 22 > 32""".stripMargin
      graph shouldBe showAsGraph(tree)
    }

    "list all sub-trees" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      val expected = """0 > 11 > 20 > 30
                       |0 > 12 > 21 > 31
                       |0 > 12 > 22 > 32
                       |
                       |11 > 20 > 30
                       |
                       |20 > 30
                       |
                       |30
                       |
                       |12 > 21 > 31
                       |12 > 22 > 32
                       |
                       |21 > 31
                       |
                       |31
                       |
                       |22 > 32
                       |
                       |32""".stripMargin

      val trees: List[Tree[Int]] = tree.trees
      val treesGraph = trees.map(showAsGraph).mkString("\n\n")

      treesGraph should be(expected)
    }
  }

}