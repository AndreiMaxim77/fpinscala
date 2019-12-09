package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

    def size[A](node: Tree[A]): Int = node match {
        case Leaf => 1
        case Branch => size(node.left) + size(node.right) + 1
    }

    def searchMax[A](node:Tree[A]): A = node match {
        case Leaf => _.value
        case Branch => searchMax(node.left).max(searchMax(node.right))
    }
       
    def maxDepth[A](node:Tree[A]): Int = {
        def searchDepth (node:Tree[A], d: Int): Int = node match {
        case Leaf => d + 1
        case Branch => searchDepth(node.left, d+1).max(searchDepth(node.right, d+1))
        }
        searchDepth(node, 0)
    }

    def mapTree[A](node:Tree[A], f:A => A) = node match {
        case Leaf => Leaf(f(node.value))
        case Branch => Branch(mapTree(node.left, f), mapTree(node.right, f))
    }

    def fold[A,B,C](node:Tree[A], z:(A,C) => C)(f: (A, A) => B) = node match {
        case Leaf(value) => z(value)
        case Branch(left, right) => f(fold(left, z)(f), fold(right, z)(f))
    }

    def foldSize[A](node: Tree[A]): Int = fold(node,  _ => 1)((a,b) => a + b + 1) 
    // node match {
    //     case Leaf => 1
    //     case Branch => count(node.left) + count(node.right) + 1
    // }

    def foldSearchMax[A](node:Tree[A]): A = fold(node, a => a)((a,b) => a.max(b))
    // node match {
    //     case Leaf => _.value
    //     case Branch => searchMax(node.left).max(searchMax(node.right))
    // }
       
    def foldMaxDepth[A](node:Tree[A]): Int = {
        def searchDepth (node:Tree[A], d: Int): Int = fold(node, )((a,b) => )
        // node match {
        // case Leaf => d + 1
        // case Branch => searchDepth(node.left, d+1).max(searchDepth(node.right, d+1))
        // }
        searchDepth(node, 0)
    }

    def foldMapTree[A](node:Tree[A], f:A => A) = fold(node, Leaf.apply)(Branch.apply)
    // node match {
    //     case Leaf => Leaf(f(node.value))
    //     case Branch => Branch(mapTree(node.left, f), mapTree(node.right, f))
    // }
}