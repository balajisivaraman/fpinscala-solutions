package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
      case None    => None
      case Some(a) => Some(f(a))
    }

  def getOrElse[B>:A](default: => B): B = this match {
      case None    => default
      case Some(a) => a
    }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
    val a: Option[Option[A]] = this.map(Some(_))
    a.getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val m = mean(xs)
    m.flatMap(m1 => mean(xs.map(x => math.pow(x - m1, 2))))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
      case (_, None)           => None
      case (None, _)           => None
      case (Some(v1),Some(v2)) => Some(f(v1,v2))
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldLeft[Option[List[A]]](None)(map2(_, _)((la, a) => a :: la))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldLeft[Option[List[B]]](None)((olb, a) => map2(olb, f(a))((lb, b) => b :: lb))

  def sequenceUsingTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(a => a)
}
