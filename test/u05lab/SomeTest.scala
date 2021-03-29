package u05lab

import org.junit.jupiter.api.Assertions.{assertEquals, assertThrows}
import org.junit.jupiter.api.Test
import u05lab.code._


class SomeTest {
  @Test
  def testZipRight() {
    val l = List("a","b","c")
    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List(("a",0), ("b",1), ("c",2)), l.zipRight)
  }

  @Test
  def testPartition() {
    val l = List("aaaa","bbbbb","c","dddd", "e")
    assertEquals((List.nil, List.nil), List.nil[AnyRef].partition(_ => true))
    assertEquals((List("aaaa","bbbbb", "dddd"), List("c", "e")), l.partition(s => s.length > 3))
  }

  @Test
  def testSpan() {
    val l = List("aaaa","bbbbb","c", "dddd", "e")
    assertEquals((List.nil, List.nil), List.nil[AnyRef].span(_ => true))
    assertEquals((List("aaaa","bbbbb"), List("c", "dddd", "e")), l.span(s => s.length > 3))
  }

  @Test
  def testReduce() {
    assertThrows[IllegalStateException](List.nil.reduce(_))
    assertEquals("a", List("a").reduce(_+_))
    assertEquals("abc", List("a","b","c").reduce(_+_))
  }
}