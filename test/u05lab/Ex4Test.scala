package u05lab

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

import scala.annotation.tailrec

class Ex4Test {
  @Test
  def ex4() {
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      @tailrec
      def _sequence(lst: List[Option[A]], acc: List[A]): Option[List[A]] = lst match {
        case Some(head) :: tail => _sequence(tail, acc.appended(head))
        case None :: _ => None
        case Nil => Some(acc)
      }
      _sequence(a, List())
    }

    assertEquals(Some(List()), sequence(List()))
    assertEquals(Some(List(1,2)), sequence(List(Some(1),Some(2))))
    assertEquals(None, sequence(List(Some(1),Some(2),None)))
    assertEquals(None, sequence(List(Some(1),None,Some(2))))
    assertEquals(None, sequence(List(None,Some(1),Some(2))))
  }
}
