import org.scalatest.FunSuite
import scala.collection.immutable.Queue
import scala.collection.immutable.ListMap
//import IgnoreList.ignoreList
import Main._

import org.scalatest.FunSuite

class TestClassAll extends FunSuite {

  def testQueueEmpty(description: String) = {
    test(description) {
      var queue: Queue[String] = Queue.empty[String]

      assert(queue.isEmpty)
    }
  }

  def testSortMapSize(description: String) = {
    test(description) {
      val queue = Queue("a", "b", "c", "d", "d", "d", "a")
      val thingMap = queue.groupBy(identity).mapValues(_.size)
      assert(thingMap.size === 4)
    }
  }

  def testSortMap(description: String) = {
    test(description) {
      val queue = Queue("a", "b", "c", "d", "d", "d", "a")
      val thingMap = queue.groupBy(identity).mapValues(_.size)
      val sortedThingMap = ListMap(thingMap.toSeq.sortWith(_._2 < _._2): _*)
      val sortedString = sortedThingMap.toString
      assert(sortedString === "ListMap(b -> 1, c -> 1, a -> 2, d -> 3)")
    }
  }

  def testQueueSize(description: String, times: Int) = {
    test(description) {
      var queue = Queue("a", "b", "c", "d", "e")
      for (w <- 0 to times) {
        queue = queue.enqueue("a")
        if (queue.length > 5) {
          var dumped = queue.dequeue
          queue = dumped._2
        }
      }
      assert(queue.size === 5)

    }
  }

  testQueueEmpty("Checks whether it initates as empty")
  testSortMapSize("Checks whether the map is being made properly")
  testSortMap("Test whether the map gets sorted")
  testQueueSize("Tests whether or not size is maintained", 10)
}

