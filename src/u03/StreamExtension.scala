package u03

import u03.Streams.Stream

object StreamExtension {
  /*
  //5
  def drop[A](stream: Stream[A])(n: Int): Stream[A] = stream match {
    case Cons(_,t) if n>0 => drop(t())(n-1)
    case _ => stream
  }
*/
  //6
  def constant[A](k: A): Stream[A] = Stream.iterate(k)(_ => k)
  def generate[A](elem: => A): Stream[A] = Stream.iterate(elem)(x=>x)

  def fibonacci( n : Int) : Int = n match {
    case 0 | 1 => n
    case _ => fibonacci( n-1 ) + fibonacci( n-2 )
  }
  //7
  def fibs:Stream[Int]=Stream.iterate(0) (fibonacci)
}
