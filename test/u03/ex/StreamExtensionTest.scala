package u03.ex

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Lists.List.{Cons, Nil}
import u03.StreamExtension
import u03.StreamExtension.{fibonacci}
import u03.Streams.Stream
import u03.Streams.Stream.take
class StreamExtesionTest {


  @Test def testDrop(): Unit = {
    val stream = Stream.take(Stream.iterate(0)(_+1))(10)
    val list1= Cons(6, Cons(7, Cons(8, Cons(9, Nil()))))
    val list2= Cons(9, Nil())
    assertEquals(list1, Stream.toList(StreamExtension.drop(stream)(6)))
    assertEquals(list2, Stream.toList(StreamExtension.drop(stream)(9)))
  }

  @Test def testConstant(): Unit = {
    val x:Int = 12
    assertEquals(Cons(x, Cons(x, Cons(x, Cons(x, Cons(x, Nil ()))))),
                  Stream.toList(Stream.take(StreamExtension.constant(x))(5)))
    assertEquals(Nil(), Stream.toList(Stream.take(StreamExtension.constant(x))(0)))
  }
  @Test def testFibonacci(): Unit = {
    val list1= Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Nil()))))))
    assertEquals(list1, Stream.toList(take(StreamExtension.fibonacci(0))(6)))
  }
}