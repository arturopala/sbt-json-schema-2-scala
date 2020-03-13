package uk.gov.hmrc.jsonschema2scala.utils

import scala.annotation.tailrec

sealed trait Tree[+T] {

  /** The number of the nodes in the tree */
  val size: Int
}

object Tree {

  def apply[T](node: T): Node[T] = Node(node, Nil)
  def apply[T](node: T, branch: Node[T], others: Node[T]*): Node[T] = Node(node, branch :: others.toList)
  def apply[T](node: T, subtrees: List[Node[T]]): Node[T] = Node(node, subtrees)

  /** An empty Tree */
  case object empty extends Tree[Nothing] {

    override val size: Int = 0
  }

  /** Concrete node of the Tree */
  case class Node[T] private (node: T, subtrees: List[Node[T]]) extends Tree[T] {

    /** The number of the nodes in the tree */
    override val size: Int =
      1 + subtrees.map(_.size).sum
  }

  implicit class TreeOps[T](tree: Tree[T]) {

    // NODES

    /** List of all the node's values in the tree, presented depth first. */
    final def nodes: List[T] =
      tree match {
        case `empty` => Nil

        case Node(node, subtrees) =>
          subtrees match {
            case Nil => List(node)
            case _ =>
              node :: subtrees.flatMap(_.nodes)
          }

      }

    /** List of all the node's values in the tree, presented depth first. Uses tail safe method. */
    final def nodesTS: List[T] = nodesTS(_ => true)

    /** List of filtered node's values in the tree, presented depth first. Uses tail safe method. */
    final def nodesTS(filter: T => Boolean): List[T] = tree match {
      case Node(node, subtrees) if filter(node) => listNodes(filter, List(node), subtrees)
      case _                                    => Nil
    }

    @tailrec
    private def listNodes(filter: T => Boolean, result: List[T], remaining: List[Node[T]]): List[T] =
      remaining match {
        case Nil => result.reverse
        case Node(node, subtrees) :: xs =>
          if (filter(node)) listNodes(filter, node :: result, subtrees ::: xs)
          else listNodes(filter, result, subtrees ::: xs)
      }

    /** Stream of the node's values in the tree, presented depth first */
    final def nodeStream: Stream[T] = nodeStream(_ => true)

    /** Filtered stream of the node's values in the tree, presented depth first. */
    final def nodeStream(filter: T => Boolean): Stream[T] = streamNodes(filter, tree, Nil)

    private def streamNodes(filter: T => Boolean, tree: Tree[T], remaining: List[Node[T]]): Stream[T] =
      tree match {
        case `empty` => Stream.empty

        case Node(node, subtrees) =>
          def continue: Stream[T] = subtrees match {
            case x :: xs =>
              streamNodes(filter, x, xs ::: remaining)

            case Nil =>
              remaining match {
                case y :: ys => streamNodes(filter, y, ys)
                case Nil     => Stream.empty
              }
          }
          if (filter(node)) Stream.cons(node, continue) else continue
      }

    // SUB-TREES

    /** Selects a sub-tree starting at the given path, if any. */
    @tailrec
    final def select[T1 <: T](path: List[T1]): Tree[T] =
      tree match {
        case `empty` => empty

        case Node(node, subtrees) =>
          path match {
            case `node` :: Nil => tree

            case `node` :: (xxs @ x :: _) =>
              subtrees
                .collectFirst {
                  case node @ Node(`x`, _) => node
                }
                .getOrElse(empty)
                .select(xxs)

            case _ => empty
          }
      }

    /** List all the possible subtrees of this tree inclusive. */
    final def trees: List[Tree[T]] = tree match {
      case `empty` => Nil
      case node @ Node(_, subtrees) =>
        node :: subtrees.flatMap(_.trees)
    }

    /** List all the possible subtrees of this tree inclusive.
      * Uses tail safe method. Outcome is the same as of `trees`. */
    final def treesTS: List[Tree[T]] = treesTS(_ => true)

    /** List filtered subtrees of this tree inclusive.
      * Uses tail safe method. Outcome is the same as of `trees`. */
    final def treesTS(filter: Tree[T] => Boolean): List[Tree[T]] = tree match {
      case node @ Node(_, subtrees) if filter(node) => listTrees(filter, List(node), subtrees)
      case `empty`                                  => List(empty)
    }

    @tailrec
    private def listTrees(filter: Tree[T] => Boolean, result: List[Node[T]], remaining: List[Node[T]]): List[Node[T]] =
      remaining match {
        case Nil => result.reverse
        case (node @ Node(_, subtrees)) :: xs =>
          if (filter(node)) listTrees(filter, node :: result, subtrees ::: xs)
          else listTrees(filter, result, subtrees ::: xs)
      }

    // BRANCHES

    /** List all the branches of this tree starting at the root. */
    final def branches: List[List[T]] =
      tree match {
        case `empty` => Nil

        case Node(node, subtrees) =>
          subtrees match {
            case Nil => List(List(node))
            case _ =>
              subtrees.flatMap(_.branches).map(node :: _)
          }

      }

    /** List all the branches of this tree starting at the root.
      * Uses tail safe method. Outcome is the same as `branches`. */
    final def branchesTS: List[List[T]] = branchesTS(_ => true)

    /** List filtered branches of this tree starting at the root.
      * Uses tail safe method. Outcome is the same as `branches`.
      * Warning: An argument to the filter function is a REVERSED branch.
      */
    final def branchesTS(filter: List[T] => Boolean): List[List[T]] = tree match {
      case `empty`              => Nil
      case Node(node, subtrees) => listBranches(filter, Nil, subtrees.map((List(node), _)))
    }

    @tailrec
    private def listBranches(
      filter: List[T] => Boolean,
      result: List[List[T]],
      remaining: List[(List[T], Node[T])]): List[List[T]] =
      remaining match {
        case Nil => result.reverse
        case (acc, Node(node, subtrees)) :: xs =>
          val branch = node :: acc
          subtrees match {
            case Nil if filter(branch) => listBranches(filter, branch.reverse :: result, xs)
            case _                     => listBranches(filter, result, subtrees.map((branch, _)) ::: xs)
          }
      }

    /** Stream of all the branches of this tree starting at the root. */
    def branchStream: Stream[List[T]] = streamBranches(tree, Nil, Nil)

    private def streamBranches(tree: Tree[T], acc: List[T], remaining: List[(List[T], Node[T])]): Stream[List[T]] =
      tree match {
        case `empty` =>
          Stream.cons(acc.reverse, remaining match {
            case (acc2, y) :: ys => streamBranches(y, acc2, ys)
            case Nil             => Stream.empty
          })

        case Node(node, subtrees) =>
          subtrees match {
            case x :: Nil =>
              streamBranches(x, node :: acc, remaining)

            case x :: xs =>
              streamBranches(x, node :: acc, (acc, Node(node, xs)) :: remaining)

            case Nil =>
              streamBranches(empty, node :: acc, remaining)
          }
      }

    /** Checks if the given path is a valid prefix of any branch of this tree. */
    @tailrec
    final def contains[T1 <: T](path: List[T1]): Boolean =
      tree match {
        case `empty` => false

        case Node(node, subtrees) =>
          path match {
            case `node` :: Nil => true

            case `node` :: (xxs @ x :: _) =>
              subtrees
                .collectFirst {
                  case node @ Node(`x`, _) => node
                }
                .getOrElse(empty)
                .contains(xxs)

            case _ => false
          }
      }

    /** Returns the number of distinct branches starting at the root of the tree. */
    final def numberOfBranches: Int = tree match {
      case `empty` => 0
      case Node(_, subtrees) =>
        subtrees match {
          case Nil => 1
          case _   => countBranches(0, subtrees)
        }
    }

    @tailrec
    private def countBranches(acc: Int, remaining: List[Node[T]]): Int = remaining match {
      case Nil => acc
      case Node(_, subtrees) :: xs =>
        subtrees match {
          case Nil => countBranches(acc + 1, xs)
          case _   => countBranches(acc, subtrees ::: xs)
        }
    }

    /** Inserts a new branch and returns updated tree
      * New branch must start with the existing root element of tree,
      * otherwise nothing will happen. */
    final def insert[T1 <: T](branch: List[T1]): Tree[T] =
      branch match {
        case x :: xs =>
          tree match {
            case `empty`             => insertBranch(Tree(x), xs)
            case node @ Node(`x`, _) => insertBranch(node, xs)
            case _                   => tree
          }

        case _ => tree
      }

    private def insertBranch[T1 <: T](tree: Node[T], branch: List[T1]): Node[T] =
      branch match {
        case x :: xs =>
          tree.subtrees.partition(_.node == x) match {

            case (Nil, bs) =>
              val c = insertBranch(Tree(x), xs)
              Node(tree.node, c :: bs)

            case (as, bs) =>
              as match {

                case a :: Nil =>
                  val c = insertBranch(a, xs)
                  Node(tree.node, c :: bs)

                case _ =>
                  val cs = as.map(insertBranch(_, xs))
                  Node(tree.node, cs ::: bs)
              }
          }

        case Nil => tree
      }
  }

}
