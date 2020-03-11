package uk.gov.hmrc.jsonschema2scala

import org.scalatest.{Matchers, WordSpec}
import uk.gov.hmrc.jsonschema2scala.utils.Tree

class TreeSpec extends WordSpec with Matchers {

  "Tree" should {
    "create an empty Tree" in {
      val tree = Tree.empty
      tree.size shouldBe 0
      tree.numberOfBranches shouldBe 0
    }

    "create a single node Tree" in {
      val tree = Tree(0)
      tree shouldBe Tree.Node(0, Nil)
      tree.size shouldBe 1
      tree.numberOfBranches shouldBe 1
    }

    "create a multi-branch Tree" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree.size shouldBe 9
      tree.numberOfBranches shouldBe 3
    }

    "insert new branch to an empty Tree" in {
      val tree = Tree(0)
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

    "list all branches" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      val expected = """0 > 11 > 20 > 30
                       |0 > 12 > 21 > 31
                       |0 > 12 > 22 > 32""".stripMargin
      tree.branches.map(_.mkString(" > ")).mkString("\n") should be(expected)
      tree.branchesTS.map(_.mkString(" > ")).mkString("\n") should be(expected)
    }

    "stream all branches" in {
      val tree = Tree(0, Tree(11, Tree(20, Tree(30))), Tree(12, Tree(21, Tree(31)), Tree(22, Tree(32))))
      tree.branchStream.map(_.mkString(" > ")).mkString("\n") should be("""0 > 11 > 20 > 30
                                                                          |0 > 12 > 21 > 31
                                                                          |0 > 12 > 22 > 32""".stripMargin)
    }
  }

}
