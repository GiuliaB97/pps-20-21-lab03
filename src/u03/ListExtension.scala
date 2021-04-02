  package u03

import u02.Modules.Person.Teacher
  import u03.Lists.List
  import u03.Lists.List.{Cons, Nil, append}

  object ListExtension extends App {
    //ex 1.a
    def drop[A](l: List[A], n:Int): List[A]= l match {
      case Cons(_, t) if n > 0 => drop(t, n-1)
      case _ => l
    }

    //ex 1.b
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()
    }

    //ex 1.c
    def map[A,B](l: List[A])(f: A=>B): List[B] = flatMap(l)(a=> Cons(f(a), Nil()))

    //ex 1.d
    def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l) (a=> if(f(a)) Cons(a, Nil()) else Nil())

    //ex 2
    def max(l: List[Int]): Option[Int] = l match{
      case Cons(h, t) => filterViaFlatMap(t)(x => x > h) match{
        case Cons(h, _)=> max(t)
        case _ => Some(h)
      }
      case _ => None
    }
    //  3
    def filterCourse (l : List[Teacher]):List[String] = flatMap(l)(l1=>l1 match{
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    })

    // 4.a
    def foldLeft(l: List[Int])(z: Int)(f: (Int, Int) => Int): Int = l match{
      case Cons(h, t)=> foldLeft(t)(f(z,h))(f)
      case _ => z
    }

    //4.b
    def foldRight(l: List[Int]) (z: Int)(f: (Int, Int) => Int): Int = l match{
      case Cons(h,t)=> f(h, foldRight(t)(z)(f))
      case _=> z
    }
  }
