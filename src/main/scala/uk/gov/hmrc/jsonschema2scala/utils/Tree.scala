package uk.gov.hmrc.jsonschema2scala.utils

import scala.annotation.tailrec

/** Immutable tree-like data structure, each node might have a value and its own subtrees */
sealed trait Tree[+T] {

  /** The number of the nodes in the tree */
  val size: Int

  /** The number of distinct branches starting at the root of the tree. */
  val numberOfBranches: Int
}

object Tree {

  def apply[T](): Tree[T] = empty
  def apply[T](node: T): Node[T] = Node(node, Nil)
  def apply[T](node: T, branch: Node[T], others: Node[T]*): Node[T] = Node(node, branch :: others.toList)
  def apply[T](node: T, subtrees: List[Node[T]]): Node[T] = Node(node, subtrees)

  private final def buildFromList[K](list: List[(Int, K)], result: List[Node[K]]): Tree[K] = list match {
    case Nil => result.headOption.getOrElse(empty)
    case (size, value) :: xs =>
      buildFromList(xs, Node(value, result.take(size)) :: result.drop(size))
  }

  /** An empty Tree */
  case object empty extends Tree[Nothing] {

    override val size: Int = 0
    override val numberOfBranches: Int = 0
  }

  /** Concrete node of the Tree, consisting of a value and subtrees */
  case class Node[+T] private (value: T, subtrees: List[Node[T]]) extends Tree[T] {

    /** The number of the nodes in the tree */
    override val size: Int =
      1 + subtrees.map(_.size).sum

    /** The number of distinct branches starting at the root of the tree. */
    override val numberOfBranches: Int =
      Math.max(1, subtrees.map(_.numberOfBranches).sum)
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, Nil)
  }

  /** Functions of the Tree */
  implicit class TreeOps[T](tree: Tree[T]) {

    // NODES

    /** List of all the node's values in the tree, presented depth first. */
    final def nodesUnsafe: List[T] =
      tree match {
        case `empty` => Nil

        case Node(node, subtrees) =>
          subtrees match {
            case Nil => List(node)
            case _ =>
              node :: subtrees.flatMap(_.nodesUnsafe)
          }

      }

    /** List of all the node's values in the tree, presented depth first. Uses tail safe method. */
    final def nodes: List[T] = nodes(_ => true)

    /** List of filtered node's values in the tree, presented depth first. Uses tail safe method. */
    final def nodes(filter: T => Boolean): List[T] = tree match {
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

    /** Lazy stream of the node's values in the tree, presented depth first */
    final def nodeStream: Stream[T] = nodeStream(_ => true)

    /** Filtered lazy stream of the node's values in the tree, presented depth first. */
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
    final def treesUnsafe: List[Tree[T]] = tree match {
      case `empty` => Nil
      case node @ Node(_, subtrees) =>
        node :: subtrees.flatMap(_.treesUnsafe)
    }

    /** List all the possible subtrees of this tree inclusive.
      * Uses tail safe method. Outcome is the same as of `trees`. */
    final def trees: List[Tree[T]] = trees(_ => true)

    /** List filtered subtrees of this tree inclusive.
      * Uses tail safe method. Outcome is the same as of `trees`. */
    final def trees(filter: Tree[T] => Boolean): List[Tree[T]] = tree match {
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
    final def branchesUnsafe: List[List[T]] =
      tree match {
        case `empty` => Nil

        case Node(node, subtrees) =>
          subtrees match {
            case Nil => List(List(node))
            case _ =>
              subtrees.flatMap(_.branchesUnsafe).map(node :: _)
          }

      }

    /** List all the branches of this tree starting at the root.
      * Uses tail safe method. Outcome is the same as `branches`. */
    final def branches: List[List[T]] = branches(_ => true)

    /** List filtered branches of this tree starting at the root.
      * Uses tail safe method. Outcome is the same as `branches`.
      * Warning: An argument to the filter function is a REVERSED branch.
      */
    final def branches(filter: List[T] => Boolean): List[List[T]] = tree match {
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

    /** Filtered lazy stream of all the branches of this tree starting at the root. */
    def branchStream: Stream[List[T]] = branchStream(_ => true)

    def branchStream(filter: List[T] => Boolean): Stream[List[T]] = streamBranches(filter, tree, Nil, Nil)

    private def streamBranches(
      filter: List[T] => Boolean,
      tree: Tree[T],
      acc: List[T],
      remaining: List[(List[T], Node[T])]): Stream[List[T]] =
      tree match {
        case `empty` => Stream.empty

        case Node(node, subtrees) =>
          subtrees match {
            case x :: Nil =>
              streamBranches(filter, x, node :: acc, remaining)

            case x :: xs =>
              streamBranches(filter, x, node :: acc, (acc, Node(node, xs)) :: remaining)

            case Nil =>
              val branch = node :: acc
              def continue: Stream[List[T]] = remaining match {
                case (acc2, y) :: ys => streamBranches(filter, y, acc2, ys)
                case Nil             => Stream.empty
              }
              if (filter(branch)) Stream.cons(branch.reverse, continue)
              else continue
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

    /** Returns the number of distinct branches accepted by the filter, starting at the root of the tree. */
    final def countBranches(filter: List[T] => Boolean): Int = tree match {
      case `empty` => 0
      case Node(node, subtrees) =>
        subtrees match {
          case Nil => 1
          case _   => countBranches(filter, 0, subtrees.map((List(node), _)))
        }
    }

    @tailrec
    private def countBranches(filter: List[T] => Boolean, result: Int, remaining: List[(List[T], Node[T])]): Int =
      remaining match {
        case Nil => result
        case (acc, Node(node, subtrees)) :: xs =>
          val branch = node :: acc
          subtrees match {
            case Nil if filter(branch) => countBranches(filter, 1 + result, xs)
            case _                     => countBranches(filter, result, subtrees.map((branch, _)) ::: xs)
          }
      }

    // MODIFICATION

    /** Inserts a new node holding the value and returns updated tree */
    final def insert[T1 <: T](value: T1): Tree[T] =
      tree match {
        case `empty`              => Node(value, Nil)
        case Node(node, subtrees) => Node(node, Node(value, Nil) :: subtrees)
      }

    /** Inserts a new sub-tree and returns updated tree */
    final def insert[T1 <: T](subtree: Tree[T1]): Tree[T] =
      tree match {
        case `empty` => subtree
        case Node(value, subtrees) =>
          subtree match {
            case `empty`        => tree
            case node: Node[T1] => Node(value, node :: subtrees)
          }
      }

    /** Inserts a new branch of values and returns updated tree
      * New branch must start with the existing root element of tree,
      * otherwise the tree stays intact. */
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
          tree.subtrees.partition(_.value == x) match {

            case (Nil, bs) =>
              val c = insertBranch(Tree(x), xs)
              Node(tree.value, c :: bs)

            case (as, bs) =>
              as match {

                case a :: Nil =>
                  val c = insertBranch(a, xs)
                  Node(tree.value, c :: bs)

                case _ =>
                  val cs = as.map(insertBranch(_, xs))
                  Node(tree.value, cs ::: bs)
              }
          }

        case Nil => tree
      }

    // TRANSFORMATION

    /** Maps all nodes of the tree using provided function and returns a new tree. */
    final def mapUnsafe[K](f: T => K): Tree[K] = tree match {
      case `empty` => empty
      case node: Node[T] =>
        def mapNode(n: Node[T]): Node[K] = Node(f(n.value), n.subtrees.map(mapNode))
        mapNode(node)
    }

    /** Maps all nodes of the tree using provided function and returns a new tree.
      * Uses tail safe method. */
    final def map[K](f: T => K): Tree[K] = tree match {
      case `empty` => empty
      case Node(value, subtrees) =>
        val list: List[(Int, K)] = mapNodes(f, List((subtrees.size, f(value))), subtrees)
        buildFromList(list, Nil)
    }

    @tailrec
    private def mapNodes[K](f: T => K, result: List[(Int, K)], remaining: List[Node[T]]): List[(Int, K)] =
      remaining match {
        case Nil => result
        case Node(node, subtrees) :: xs =>
          subtrees match {
            case Nil => mapNodes(f, (subtrees.size, f(node)) :: result, subtrees ::: xs)
            case _   => mapNodes(f, (subtrees.size, f(node)) :: result, subtrees ::: xs)
          }

      }

    // VISUALIZATION

    /** Makes a String representation of the tree.
      * Uses tail safe method.
      * @param show function to render a node value
      * @param nodeSeparator string to separate nodes
      * @param branchStart string to add at the start of each branch
      * @param branchEnd string to add at the end of each branch
      * @param branchSeparator string to separate branches
      */
    final def mkString(
      show: T => String,
      nodeSeparator: String,
      branchSeparator: String,
      branchStart: String,
      branchEnd: String): String =
      tree match {
        case `empty`         => ""
        case Node(node, Nil) => branchStart + show(node) + branchEnd
        case Node(node, subtrees) =>
          val s = show(node)
          mkString(
            show,
            nodeSeparator,
            branchSeparator,
            branchEnd,
            new StringBuilder().append(branchStart).append(s),
            subtrees.map((branchStart + s, _)),
            newBranch = false
          ).mkString
      }

    @tailrec
    private def mkString(
      show: T => String,
      nodeSeparator: String,
      branchSeparator: String,
      branchEnd: String,
      result: StringBuilder,
      remaining: List[(String, Node[T])],
      newBranch: Boolean): StringBuilder =
      remaining match {
        case Nil => result
        case (prefix, Node(node, subtrees)) :: xs =>
          val s = show(node)
          val builder = (if (newBranch) result.append(branchSeparator).append(prefix) else result)
            .append(nodeSeparator)
            .append(s)
          subtrees match {
            case Nil =>
              mkString(show, nodeSeparator, branchSeparator, branchEnd, builder.append(branchEnd), xs, newBranch = true)
            case _ =>
              mkString(
                show,
                nodeSeparator,
                branchSeparator,
                branchEnd,
                builder,
                subtrees.map((prefix + nodeSeparator + s, _)) ::: xs,
                newBranch = false)
          }
      }
  }

}
