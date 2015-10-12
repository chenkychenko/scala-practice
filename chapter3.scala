sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(x,xs) => Cons(x, append(xs, a2))
    }
  }

  // exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_,xs) => xs
  }
    
  // exercise 3.3
  def setHead[A](v: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_,xs) => Cons(v, xs)
  }
    
  // exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Cons(x,xs) => drop(xs, n-1)
      case Nil => Nil
    }
  }
  
  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x,xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  //exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,Nil) => Nil
    case Cons(x,xs) => Cons(x, init(xs))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Doube = ds match {
    case Nil => 1.0
    case Cons(x,xs) => x * product(xs)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x,xs) => f(foldRight(xs,z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns,0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns,1)((x,y) => x * y)

  // exercise 3.7:
  // can product implemented with foldRight halt the recursion and return 0.0
  // if it encounters a 0.0? => No, because it's just called foldRight
  // Short-circuiting when calling foldRight with a large list...

  // exercise 3.8
  // What happens when you pass Nil and Cons themselves to foldRight:
  // foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

  // exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as,0)((_,acc) => acc + 1)
  }

  // foldRight demonstrated for sum function
  foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x,y) => x + y)
  1 + foldRight(Cons(2, Cons(3, Nil)), 0)((x,y) => x + y)
  1 + 2 + foldRight(Cons(3, Nil), 0)((x,y) => x + y)
  1 + 2 + 3 + foldRight(Cons(Nil, 0), 0)((x,y) => x + y)
  1 + 2 + 3 + 0

  // foldRight demonstrated for length function
  foldRight(Cons(1, Cons(2, Cons(3, Nil))),0)((x,y) => y + 1)
  1 + foldRight(Cons(2, Cons(3, Nil)), 0)((x,y) => y + 1)
  1 + 1 + foldRight(Cons(3, Nil), 0)((x,y) => y + 1)
  1 + 1 + 1 + foldRight(Cons(Nil, 0), 0)((x,y) => y + 1)
  1 + 1 + 1 + 0

  // exercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs, f(x,z))(f)
    }
  }

  // exercise 3.11
  def sum3(l: List[Int]) = foldLeft(l,0)(_ + _)

  def product3(l: List[Double]) = foldLeft(l,1.0)(_ * _)

  def length2[A](l: List[A]) = foldLeft(l,0)((acc,h) => acc + 1)

  // exercise 3.12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l,List[A]())((x,y) => Cons(y, x))
  }

  // exercise 3.13
  
}













