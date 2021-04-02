package u03

import u03.Lists.List._
import u03.ListsMain.l2

import scala.Console.println

object Lists {

  // A generic linkedlist
  sealed trait List[E]

  // a companion object (i.e., module) for List
  object List {
    case class Cons[E](head: E, tail: List[E]) extends List[E]
    case class Nil[E]() extends List[E]

    def sum(l: List[Int]): Int = l match {
      case Cons(h, t) => h + sum(t)
      case _ => 0
    }

    def append[A](l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
      case (Cons(h, t), l2) => Cons(h, append(t, l2))
      case _ => l2
    }

    def map[A,B](l: List[A])(mapper: A=>B): List[B] = l match {
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()
    }

    def filter[A](l1: List[A])(pred: A=>Boolean): List[A] = l1 match {
      case Cons(h,t) if (pred(h)) => Cons(h, filter(t)(pred))
      case Cons(_,t) => filter(t)(pred)
      case Nil() => Nil()
    }

    val x = l2 match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    def tail[A](l: List[A]): List[A] =
      l match {
        case Nil() => sys.error("tail of empty list")
        case Cons(_, t) => t
      }
    def setHead[A](l: List[A], h: A): List[A] =
      l match {
        case Nil() => sys.error("setHead on empty list")
        case Cons(_, t) => Cons(h, t)
      }

    def init[A](l: List[A]): List[A] =
      l match {
        case Nil() => sys.error("init of empty list")
        case Cons(_, Nil()) => Nil()
        case Cons(h, t) => Cons(h, init(t))
      }
  }

}

object ListsMain extends App {
  import Lists._
  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))

  println("sum " +List.sum(l)) // 60
  println("append " + append(Cons(5, Nil()), l)) // 5,10,20,30
  println("filter" + filter[Int](l)(_ >=20)) // 20,30

  val l2 = List.Cons(1, List.Cons(2, List.Cons(3,  List.Cons(4,  List.Cons(5, List.Nil())))))
  println("sum " + List.sum(l2)) // 60
  println("x function "+ x)
  println("tail "+ tail(List.Cons(1, List.Cons(2, List.Cons(3, List.Nil())))))
  println("tail "+ tail(List.Cons(0, List.Nil())))
  println("head " + setHead(List.Cons(1, List.Cons(2, List.Cons(3, List.Nil()))), 3))
  println("init " + init(List.Cons(1, List.Cons(2, List.Cons(3, List.Nil())))))
}