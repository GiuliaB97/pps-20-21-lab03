package u03


import u03.Streams.Stream
import u03.Streams.Stream._
object StreamExtension {

    //5
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = stream match {
      case Cons(_, tail) if n>0 => drop(tail())(n-1)
      case _ => stream
    }
    //6
    def constant[A](k: A): Stream[A] = Stream.iterate(k)(_ => k)

    def fibonacci( n : Int) : Stream[Int]  = {
      def _fib( p: Int, a: Int) : Stream[Int] = cons(a,_fib(a, p+a))
      _fib(1, 0)
    }
}
