package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty      => empty[A]
    case Cons(h, t) =>
      if (n == 0) empty[A]
      else Cons(h, () => t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty      => empty[A]
    case Cons(h, t) =>
      if (n > 0) t().take(n - 1)
      else t()
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] = foldRight[Option[A]](None)((a,_) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](other: => Stream[B]): Stream[B] = foldRight(other)(cons(_, _))

  def flatmap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a,b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) =>
        if (n == 0) None
        else Some((h(), t().takeViaUnfold(n - 1)))
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case Empty => None
    }

  def zipWith[B,C](other: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, other))(s =>
      (s._1, s._2) match {
        case (_, Empty)                 => None
        case (Empty, _)                 => None
        case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()), (t1(), t2())))
    })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2))(s =>
      (s._1, s._2) match {
        case (Empty, Empty)             => None
        case (Cons(h,t), Empty)         => Some(((Some(h()), None), (t(), Empty)))
        case (Empty, Cons(h,t))         => Some(((None, Some(h())), (Empty, t())))
        case (Cons(h1,t1), Cons(h2,t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    })

  def startsWith[B >: A](s: Stream[B]): Boolean = zipWith(s)(_ == _) forAll (_ == true)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty      => None
      case Cons(h, t) => Some((cons(h(), t()), t()))
    }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def loop(f: Int, s: Int): Stream[Int] = cons(f, loop(s, f + s))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None        => empty[A]
    case Some((a,s)) => cons(a, unfold(s)(f))
  }

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  val fibsViaUnfold: Stream[Int] = unfold((0,1))(s => Some((s._1, (s._2, s._1 + s._2))))

}
