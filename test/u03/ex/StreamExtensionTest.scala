import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u03.StreamExtension.{constant, fibs}
import u03.Streams.Stream._
import u03.Streams._

class StreamTaskTest {
  private val s = take(Stream.iterate(0)(_ + 1))(10)
  private val expectedDropStream = cons(6, cons(7, cons(8, cons(9, Empty()))))
  private val expectedConstStream = toList(cons("x", cons("x", cons("x", cons("x", cons("x", Empty()))))))
  private val expectedFibonacciStream = toList(cons(0, cons(1, cons(1, cons(2, cons(5, cons(8, cons(13, Empty()))))))))

  @Test def testDrop():Unit = {
    assertEquals(toList(expectedDropStream), toList(drop(s)(6)))
    assertEquals(toList(Empty()), toList(drop(s)(10)))
    assertEquals(toList(Empty()), toList(drop(s)(15)))
    assertEquals(toList(Empty()), toList(drop(Empty())(5)))
  }

  @Test def testConstant():Unit = {
    assertEquals(expectedConstStream, toList(take(constant("x"))(5)))
  }

  @Test def testFibonacci():Unit = {
    assertEquals(expectedFibonacciStream, toList(take(fibs)(8)))
  }
}