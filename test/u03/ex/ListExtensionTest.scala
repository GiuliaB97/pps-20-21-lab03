package u03.ex
import org.junit.jupiter.api.Assertions.{assertEquals, assertNotEquals}
import org.junit.jupiter.api.Test
import u02.Modules.Person.Teacher
import u03.ListExtension
import u03.ListExtension.{drop, filterCourse, filterViaFlatMap, flatMap}
import u03.Lists.List._

class ListsTasksTest {
  private val l1 = Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))
  private val l2 = Cons(2, Cons(4, Nil()))
  private val l3 = Cons (10 , Cons (20 , Cons (30 , Nil () )) )
  private val l4 = Cons (3 , Cons (7 , Cons (1 , Cons (5 , Nil () ) ) ) )

  private val teachers: Cons[Teacher] = Cons(Teacher("Teacher1", "course1"), Cons(Teacher("Teacher2", "course2"), Nil()))


  @Test def testDrop():Unit = {
    val expectedList: Cons[Int] = Cons(2, Cons(3, Cons(4, Nil())))
    assertEquals(expectedList, l1)
    assertEquals(expectedList, drop(l1, 1))
    assertEquals(Nil(), drop(l1, 4))
  }

  @Test def testFlatMap():Unit = {
    val expectedList: Cons[Int] =  Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ()))))))
    assertNotEquals(expectedList, l3)
    assertEquals(expectedList, flatMap(l3) (v =>Cons(v+1, Cons(v+2, Nil()))))
  }

  @Test def testMap():Unit = {
    val expectedList = Cons(Cons(3,Nil()),Cons(Cons(5,Nil()),Nil()))
    assertNotEquals(expectedList, l2)
    assertEquals(expectedList, map(l2) (v => Cons(v+1, Nil())))
  }

  @Test def testFilterViaFlatMap():Unit = {
    val expectedList: Cons[Int] = Cons(2, Nil())
    assertNotEquals(expectedList, l2)
    assertEquals(expectedList, filterViaFlatMap(l2) (x=> x<=2))
  }

  @Test def testMax():Unit = {
    val expectedList = Some(30)
    assertNotEquals(expectedList, l3)
    assertEquals(expectedList, ListExtension.max(l3))
  }

  @Test def testFilterCourse():Unit = {
    val expectedList: Cons[String] = Cons("course1", Cons("course2", Nil()))
    assertNotEquals(expectedList, teachers)
    assertEquals(expectedList, filterCourse(teachers))
  }
/*
  @Test def foldLeft():Unit = {
    val expectedList = -16
    assertNotEquals(expectedList, l4)
    assertEquals(expectedList, foldLeft(l4)(0)(_ - _))
  }

  @Test def foldRight():Unit = {
    val expectedList = -8
    assertNotEquals(expectedList, l4)
    assertEquals(expectedList, foldLeft(l4)(0)(_ - _))
  }
*/
}