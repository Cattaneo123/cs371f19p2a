import org.scalatest.FunSuite
import scala.collection.immutable.Queue
import scala.collection.immutable.ListMap
import IgnoreList.ignoreList
import Main._

import org.scalatest.FunSuite

class TestClassAll extends FunSuite {

  def testPrinterSlide(des: String, hm: Int, ml: Int, lnw: Int, mf: Int) = {
    test(des) {
      val makerTest: Window = new Window(hm, ml, lnw, mf)
      var howmany1 = hm
      var minlength1 = ml
      var lastNWords1 = lnw
      var minfreq1 = mf
      val qtest1: Queue[String] = Queue.empty[String]
      assert(qtest1.isEmpty)
      val linesTestOne = scala.io.Source.fromFile(des).getLines
      val holderTestOne = linesTestOne.flatMap(line => line.split("(?U)[^\\p{Alpha}0-9']+"))
      val printerTest1: Output = new Output() {
        override def update(cloud: Queue[String]): Unit = {
          var frequency = cloud.groupBy(identity).mapValues(_.size)
          var allfreq = ListMap(frequency.toSeq.sortWith(_._2 > _._2): _*)
          var topfreq = allfreq.take(5)
          var finalfreq = topfreq.filter((t) => t._2 >= minfreq1)
          if (howmany1 > cloud.size) {

          } else {
            for ((k, v) <- finalfreq) printf("%s: %s ", k, v)
            println("")
          }
        }
      }
      assert(!holderTestOne.isEmpty)
      makerTest.slidingWindow(holderTestOne, printerTest1)
      assert(holderTestOne.isEmpty)
    }
  }

  testPrinterSlide("testingFileOne.txt", 10, 1, 200, 1)
  testPrinterSlide("testingFileTwo.txt", 5, 2, 50, 5)
  testPrinterSlide("testingFileThree.txt", 7, 4, 150, 3)

}

}
