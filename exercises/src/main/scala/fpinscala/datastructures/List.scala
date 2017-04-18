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

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

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
      case Nil         => throw new RuntimeException("tail on empty list!")
      case Cons(a, as) => as
    }

  def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil         => throw new RuntimeException("setHead on empty list!")
      case Cons(a, as) => Cons(h, as)
    }

  def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil         => Nil
      case _ if n == 0 => l
      case Cons(a, as) => drop(as, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil                 => Nil
      case Cons(a, as) if f(a) => dropWhile(as, f)
      case _                   => l
    }

  def init[A](l: List[A]): List[A] = l match {
      case Nil          => Nil
      case Cons(a, Nil) => Nil
      case Cons(a, as)  => Cons(a, init(as))
   }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => 1 + b)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil         => z
      case Cons(a, as) => foldLeft(as, f(z, a))(f)
    }

  def sumUsingFoldLeft(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def productUsingFoldLeft(ns: List[Int]) = foldLeft(ns, 1)(_ * _)
  def lengthUsingFoldLeft[A](ns: List[A]) = foldLeft(ns, 0)((length, _) => length + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def appendUsingFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def concatenate[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append(_, _))

  def addOne(l: List[Int]): List[Int] = l match {
      case Nil         => Nil
      case Cons(i, is) => Cons(i + 1, addOne(is))
    }

  def doubleToString(l: List[Double]): List[String] = l match {
      case Nil         => Nil
      case Cons(d, ds) => Cons(d.toString(), doubleToString(ds))
    }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
      case Nil         => Nil
      case Cons(a, as) => Cons(f(a), map(as)(f))
    }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil                  => Nil
      case Cons(a, as) if !f(a) => filter(as)(f)
      case Cons(a, as)          => Cons(a, filter(as)(f))
    }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
      case Nil         => Nil
      case Cons(a, as) => append(f(a), flatMap(as)(f))
    }

  def filterUsingFlatmap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  def zip(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (_, Nil)                   => Nil
      case (Nil, _)                   => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(a + b, zip(as, bs))
    }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
      case (_, Nil)                   => Nil
      case (Nil, _)                   => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a,b), zipWith(as, bs)(f))
    }

}
