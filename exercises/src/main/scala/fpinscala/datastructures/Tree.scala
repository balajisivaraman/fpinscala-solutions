package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v)             => v
      case Branch(left, right) => maximum(left).max(maximum(right))
    }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(v)             => Leaf(f(v))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
      case Leaf(v)             => l(v)
      case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }
}
