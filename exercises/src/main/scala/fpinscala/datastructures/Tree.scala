package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

    def size(): Int = {
        def count(node: Tree[A], acc: Int) = node match {
            case Leaf => acc + 1
            case Branch => count(node.left, acc+1) + count(node.right, acc+1) - acc
        }
        
    }

    def maximum(): Int = {
        def search(node:Tree[A], maxVal:Int) = {
            
        }
        search(this, 0)
    }


}