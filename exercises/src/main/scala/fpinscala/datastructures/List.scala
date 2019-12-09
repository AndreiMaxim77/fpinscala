package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def sum_tailRec(ints: List[Int], acc: Int): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => acc // The sum of the empty list is 0.
    case Cons(x,xs) => sum_tailRec(xs, acc + x) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) // how come this one works? .tail?

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions 
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def head[A](l: List[A]): A = l match {
    //case Nil => throw ElementNotFoundException
    case Cons(x, xs) => x
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def setHead2[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l 
    case x if (x < 0) => l
    case _ => drop(tail(l), n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }
  

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0) ( (_, y) => 1 + y )

  // Exercise 3.10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  // Exercise 3.11
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def prod3(ns: List[Int]) = foldLeft(ns, 1)(_ * _)
  def length2(ns: List[Int]) = foldRight(ns, (1-head(ns))) ((x,_)=> x + 1) // ?? maybe this would in fact work with foldRight
  def length3(ns: List[Int]) = foldLeft(ns, 1)((x,_)=> x + 1)

  // Exercise 3.12 - List Reverse function
  def reverse[A](xs: List[A]) = 
    //foldLeft(xs, head(xs))((x,y) => Cons(x,))
    foldLeft(xs, List[A]())((x, y) => Cons(y, x))


  // Exercise 3.13 - Write foldL in terms of foldR and vice-versa
  def foldLfromR[A,B](l: List[A], z:B)(f: (B, A)=>B): B = {
    //foldRight(as, z:B)((x:A,y:B)=>B)
    //foldRight(l, z)((a,b)=>b)
    foldRight(l, z)((a,b)=>f(b,a))
  }

  foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B
  foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B

  def foldRfromL[A,B](l: List[A], z:B)(f: (A,B)=>B) = {
    foldLeft[A,B](l, z)((a,b)=>f(b,a))
  }

    // Exercise 3.14 - Append in terms of foldLeft / foldRight
  def appendFL[A](a1: List[A], a2: List[A]): List[A] = {
    //foldLeft(reverse(a1), a2)(setHead)
    foldLeft(reverse(a1), a2)((xs,y)=>Cons(y, xs))
  }
  def appendFR[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_,_))
  }

  // Exercise 3.15 - Concatenation of a list(lists) into a single list (maybe flatMap?)
  def flatten[A](l: List[A]*): List[A] = {
    foldRight(l, List[A]())(append(_,_))
  }

  // Exercise 3.16 
  def add1[Int](l: List[Int]): List[Int] = {
    foldRight(l, Nil)((x,y)=>Cons(x+1, y))
    //foldLeft(reverse(l), Nil)((x,y)=>Cons(y+1,x))
  }

  // Exercise 3.17
  def dtoS[Double](l: List[Double]): List[String] = {
    foldRight(l, Nil)((x,y)=>Cons(x.toString, y))
  }

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil)((x,y)=>Cons(f(x),y))
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A=> Boolean): List[A] = {
    //map(as)()
    //flatten(foldRight(as, List.empty[List[A]])((a, acc) => if(f(a)) Cons(List(a), acc) else acc ) )
    foldRight(as, List[A]())((a, acc)=> if(f(a)) Cons(a, acc) else acc)

  }
  // f(head(as)) match {
  //   case True => foldRight(as, Nil)((x,y)=> if(f(y)) then Cons(x, y) else Cons(x, Nil)) // xs:Nil:ys:Nil:z => xs:ys:z
  //   case False => foldRight(as, Nil)((x,y)=> if(f(y)) then Cons(x, y) else Cons(x, Nil)).drop(1)
  // }

  // Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(map(as)(f), Nil)(append)
    //flatten(map(as)(f))
  }

  // Exercise 3.21
  def filterFlatMap[A](as: List[A])(f: A=>Boolean): List[A] = {
    flatMap(as)(a=> if(f(a)) List(a) else Nil)
  }

  def zip[A,B](as: List[A], bs: List[B]): List[(A,B)] = //zipWith(as,bs)((_,_))
  (as, bs) match {
    case (Cons(a, tailA), Cons(b, tailB)) => Cons((a,b), zip(tailA, tailB))
    case _ => Nil
  }

  def zipWith[A,B](as: List[A], bs: List[B])(f:(A,B)=>C): List[C] = (as, bs) match {
    case (Cons(a, tailA), Cons(b, tailB)) => Cons(f(a,b), zip(tailA, tailB))
    case _ => Nil
  }

  // Exercise 3.22
  def addListsElements[A](xs: List[A], ys: List[A]): List[A] = {
    map(zip(xs, yx))(_ + _)
  }

  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    func1 map (1,2,3,4) to ((1), (1,2), (1,2,3), (1,2,3,4))
    sup.map(func1).filter(_ == sub)
  }
}
