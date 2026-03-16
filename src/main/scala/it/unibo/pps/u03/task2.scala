package it.unibo.pps.u03

import u03.Sequences.*
import Sequence.*

import scala.annotation.tailrec

object task2 extends App {
  @tailrec
  def foldLeft[A,B](s: Sequence[A])(default: B)(acc: (first: B, second: A) => B) : B = s match
    case Nil() => default
    case Cons(h, t) => foldLeft(t)(acc.apply(default, h))(acc)

  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  println(foldLeft(lst)(0)(_ - _)) // -16

}
