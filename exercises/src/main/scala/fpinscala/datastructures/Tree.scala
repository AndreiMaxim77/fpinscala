package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

    def size[A](node: Tree[A]): Int = node match {
        case Leaf(_) => 1
        case Branch(left, right) => size(left) + size(right) + 1
    }

    def searchMax(node:Tree[Int]): Int = node match {
        case Leaf(value) => value
        case Branch(left, right) => searchMax(left).max(searchMax(right))
    }
       
    def maxDepth[A](node:Tree[A]): Int = node match {
        // def searchDepth (node:Tree[A], d: Int): Int = node match {
        // case Leaf(value) => d + 1
        // case Branch(left, right) => searchDepth(left, d+1).max(searchDepth(right, d+1))
        // }
        // searchDepth(node, 0)
        case Leaf(value) => 1
        case Branch(left, right) => 1 + maxDepth(left).max(maxDepth(right))
    }

    def mapTree[A](node:Tree[A], f:A => A): Tree[A] = node match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(mapTree(left, f), mapTree(right, f))
    }

    def fold[A,B](node:Tree[A], z: A => B)(f: (B, B) => B): B = node match {
        case Leaf(value) => z(value)
        case Branch(left, right) => f(fold(left, z)(f), fold(right, z)(f))
    }

    def foldSize[A](node: Tree[A]): Int = fold[A,Int](node, _ => 1)((a,b) => a + b + 1)
    // node match {
    //     case Leaf => 1
    //     case Branch => count(node.left) + count(node.right) + 1
    // }

    def foldSearchMax(node:Tree[Int]): Int = fold(node, (a: Int) => a)((a,b) => a.max(b))
    // node match {
    //     case Leaf => _.value
    //     case Branch => searchMax(node.left).max(searchMax(node.right)) 
    // }
       
    def foldDepth[A](node:Tree[A]): Int = fold(node, (_:A) => 0)((left, right) => (1 + left.max(right)))
        // node match {
            // case Leaf(value) => 1
            // case Branch(left, right) => (1 + maxDepth(left)).max(maxDepth(right) + 1)

    def foldMapTree[A,B](node:Tree[A], f:A => B) = fold[A,Tree[B]](node, (a: A) => Leaf(f(a)))((a,b) => Branch(a,b) )
    // node match {
    //     case Leaf => Leaf(f(node.value))
    //     case Branch => Branch(mapTree(node.left, f), mapTree(node.right, f))
    // }
}