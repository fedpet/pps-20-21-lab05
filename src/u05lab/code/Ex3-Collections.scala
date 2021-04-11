package u05lab.code

import java.util.concurrent.TimeUnit
import scala.collection.{immutable, mutable}
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils {
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime()-startTime, TimeUnit.NANOSECONDS)
    if(msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)
}

object CollectionsTest extends App {
  import PerformanceUtils._
  val numElements = 1000000
  val elements = 1 to numElements
  val expensiveUpdateQueriesRange = (numElements/2 - 50) to (numElements/2 + 50)
  val expensiveReadQueriesRange =
    ((numElements/2 - 50) to (numElements/2 + 50)) concat // elements "in the middle"
    (-25 to 0) concat // non-existent elements
    (numElements to numElements + 25) // non-existent elements

  implicit class ExtendedSeq[A](val list: scala.List[A]) {
    // "extension-method" with reified type B
    // example: List("a", 1).only[String](println) will print "a"
    def only[B: Manifest](f: B => Unit): Unit = {
      list.filter(e => manifest[B].runtimeClass.isAssignableFrom(e.getClass))
        .map(_.asInstanceOf[B])
        .foreach(f)
    }
  }
  def create[A, B](input: Seq[A])(creator: Seq[A] => B): B = {
    measure(creator(Seq()).getClass.getCanonicalName + " create") {
      creator(input)
    }.result
  }
  def iterableQueries[A, B](coll: scala.collection.Iterable[A]): Unit = {
    measure(coll.getClass.getCanonicalName + " size") {
      coll.size
    }
    measure(coll.getClass.getCanonicalName + " foreach") {
      coll.foreach(_ => {})
    }
    measure(coll.getClass.getCanonicalName + " last") {
      coll.last
    }
  }

  /* Linear sequences: List, ListBuffer */
  val lists = scala.List[Seq[Int] => scala.collection.Seq[Int]](
    immutable.List.apply[Int],
    immutable.Queue.apply[Int],
    immutable.Vector.apply[Int],
    immutable.ArraySeq.apply[Int],
    mutable.ListBuffer.apply[Int],
    mutable.ArrayBuffer.apply[Int]
  ).map(create(elements))
  lists.foreach(iterableQueries)
  lists.foreach(coll => {
    measure(coll.getClass.getCanonicalName + " appendedAll") {
      coll.appendedAll(elements)
    }
    measure(coll.getClass.getCanonicalName + " contains") {
      expensiveReadQueriesRange.foreach(coll.contains)
    }
    measure(coll.getClass.getCanonicalName + " updated") {
      expensiveUpdateQueriesRange.foreach(coll.updated(_, 0))
    }
    measure(coll.getClass.getCanonicalName + " reverse") {
      coll.reverse
    }
  })
  lists.only[mutable.Seq[Int]](coll => {
      measure(coll.getClass.getCanonicalName + " update") {
        expensiveUpdateQueriesRange.foreach(coll.update(_, 0))
      }
  })

  /* Sets */
  val sets = scala.List[Seq[Int] => scala.collection.Set[Int]](
    immutable.BitSet.apply,
    immutable.HashSet.apply[Int],
    immutable.TreeSet.apply[Int],
    mutable.BitSet.apply,
    mutable.HashSet.apply[Int],
    mutable.TreeSet.apply[Int]
  ).map(create(elements))
  sets.foreach(iterableQueries)
  sets.foreach(coll => {
    measure(coll.getClass.getCanonicalName + " contains") {
      expensiveReadQueriesRange.foreach(coll.contains)
    }
    val testSet = sets.head.map(_ + numElements/2)
    measure(coll.getClass.getCanonicalName + " union") {
      coll.union(testSet)
    }
    measure(coll.getClass.getCanonicalName + " diff") {
      coll.diff(testSet)
    }
    measure(coll.getClass.getCanonicalName + " intersect") {
      coll.intersect(testSet)
    }
  })
  val testSet = sets.head.map(_ + numElements/2)
  sets.only[immutable.Set[Int]](coll => {
    var mutated = coll
    measure(coll.getClass.getCanonicalName + " add") {
      testSet.foreach(el => {
        mutated = mutated + el
      })
    }
    measure(coll.getClass.getCanonicalName + " removed") {
      elements.foreach(el => {
        mutated = mutated - el
      })
    }
  })
  sets.only[mutable.Set[Int]](coll => {
    measure(coll.getClass.getCanonicalName + " add") {
      testSet.foreach(coll.add)
    }
    measure(coll.getClass.getCanonicalName + " remove") {
      elements.foreach(coll.remove)
    }
  })

  /* Maps */
  val testKeys = elements.map(_.toString)
  val testEls = testKeys.zipWithIndex
  val maps = scala.List[Seq[(String, Int)] => scala.collection.Map[String, Int]](
    immutable.HashMap.apply[String, Int],
    immutable.TreeMap.apply[String, Int],
    mutable.HashMap.apply[String, Int],
    mutable.TreeMap.apply[String, Int]
  ).map(create(testEls))
  maps.foreach(iterableQueries)
  maps.foreach(coll => {
    measure(coll.getClass.getCanonicalName + " contains") {
      expensiveReadQueriesRange.map(_.toString).foreach(coll.contains)
    }
  })
  maps.only[immutable.Map[String, Int]](coll => {
    var mutated = coll
    measure(coll.getClass.getCanonicalName + " add") {
      testEls.foreach(entry => {
        mutated = mutated + entry
      })
    }
    measure(coll.getClass.getCanonicalName + " updated") {
      testKeys.foreach(k => {
        mutated = mutated.updated(k, 0)
      })
    }
    measure(coll.getClass.getCanonicalName + " removed") {
      testKeys.foreach(k => {
        mutated = mutated.removed(k)
      })
    }
  })
  maps.only[mutable.Map[String, Int]](coll => {
    measure(coll.getClass.getCanonicalName + " add") {
      testEls.foreach(coll.addOne)
    }
    measure(coll.getClass.getCanonicalName + " update") {
      testKeys.foreach(k => coll.update(k, 0))
    }
    measure(coll.getClass.getCanonicalName + " remove") {
      testKeys.foreach(coll.remove)
    }
  })
}