package it.unibo.pps.u03

import u03.Sequences.*
import Sequence.*
import it.unibo.pps.u02.Modules.*
import it.unibo.pps.u02.Modules.Person.*

import scala.annotation.tailrec

object Task2 extends App {

  private def getCourse(p: Person): String = p match {
    case Student(_, _) => ""
    case Teacher(_, c) => c
  }

  private def giveCourses1(s: Sequence[Person]) : Sequence[String] =
    map(filter(s)(!isStudent(_)))(t => getCourse(t))

  private def giveCourses2(s: Sequence[Person]): Sequence[String] =
    flatMapT(s) {
      case Student(_, _) => Nil()
      case Teacher(_, c) => Cons(c, Nil())
    }

  val seq = Cons(Student("Mario", 2014), Cons(Teacher("Viroli", "PPS"), Cons(Teacher("Ricci", "PCD"), Cons(Teacher("Riccio", "PCD"), Nil()))))
  println(giveCourses2(seq))

  @tailrec
  private def foldLeft[A,B](s: Sequence[A])(default: B)(acc: (first: B, second: A) => B) : B = s match
    case Nil() => default
    case Cons(h, t) => foldLeft(t)(acc.apply(default, h))(acc)

  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  println(foldLeft(lst)(0)(_ - _)) // -16

  private def distinctCourses(s: Sequence[Person]): Int =
    foldLeft(distinct(giveCourses2(s)))(0)((total, _) => total + 1)

  println(distinctCourses(seq))
}
