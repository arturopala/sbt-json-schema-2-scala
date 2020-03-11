package uk.gov.hmrc.jsonschema2scala.utils

import scala.annotation.tailrec

sealed trait Tree[+T]

object Tree {

  def apply[T](node: T): Node[T] = Node(node, Nil)
  def apply[T](node: T, branch: Node[T], others: Node[T]*): Node[T] = Node(node, branch :: others.toList)
  def apply[T](node: T, subtrees: List[Node[T]]): Node[T] = Node(node, subtrees)

  case object empty extends Tree[Nothing]

  case class Node[T] private (node: T, subtrees: List[Node[T]]) extends Tree[T]

  implicit class TreeOps[T](tree: Tree[T]) {

    final def size: Int = tree match {
      case `empty`           => 0
      case Node(_, subtrees) => countNodes(1, subtrees)
    }

    @tailrec
    private def countNodes(acc: Int, remaining: List[Node[T]]): Int = remaining match {
      case Nil => acc
      case Node(_, subtrees) :: xs =>
        subtrees match {
          case Nil => countNodes(acc + 1, xs)
          case _   => countNodes(acc + 1, subtrees ::: xs)
        }
    }

    @tailrec
    final def select[T1 <: T](path: List[T1]): Tree[T] =
      tree match {
        case `empty` => empty

        case Node(node, subtrees) =>
          path match {
            case `node` :: Nil => tree

            case `node` :: x :: xs =>
              subtrees
                .collectFirst {
                  case node @ Node(`x`, _) => node
                }
                .getOrElse(empty)
                .select(x :: xs)

            case _ => empty
          }
      }

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

    final def branchesTS: List[List[T]] = tree match {
      case `empty`              => Nil
      case Node(node, subtrees) => listBranches(Nil, subtrees.map((List(node), _)))
    }

    @tailrec
    private def listBranches(result: List[List[T]], remaining: List[(List[T], Node[T])]): List[List[T]] =
      remaining match {
        case Nil => result.reverse
        case (acc, Node(node, subtrees)) :: xs =>
          subtrees match {
            case Nil => listBranches((node :: acc).reverse :: result, xs)
            case _   => listBranches(result, subtrees.map((node :: acc, _)) ::: xs)
          }
      }

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
  }

}
