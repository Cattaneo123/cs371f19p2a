import scala.collection.immutable.Queue
import scala.collection.immutable.ListMap
import sun.misc.Signal

class WindowMaker(mapSize: Int, wordSize: Int, queueSize: Int, printCounter: Int) {
  var queue: Queue[String] = Queue.empty[String]
  val maxSize: Int = queueSize
  var counter: Int = 0
  var memCounter: Int = 0

  def slidingWindow(input: Iterator[String], output: Output): Unit = {
    while (input.hasNext) {

      var word: String = input.next()
      // dumping the 1st thing when it gets too big
      if ((word.length() >= wordSize) && (word != "")) {
        queue = queue.enqueue(word.toLowerCase())
      }
      if (queue.size > maxSize) {
        var dumped = queue.dequeue
        queue = dumped._2
      }
      counter = counter + 1
      if (counter > printCounter) {
        output.update(queue)
        counter = 0
      }

      //outputs memory usage every 100 words since we don't actually have a logger easily available.
      memCounter = memCounter + 1
      if (memCounter > 100) {
        val mb = 1024 * 1024
        val runtime = Runtime.getRuntime
        println("")
        Console.err.print("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
        Console.err.print(", " + "** Free Memory:  " + runtime.freeMemory / mb)
        Console.err.print(", " + "** Total Memory: " + runtime.totalMemory / mb)
        Console.err.print(", " + "** Max Memory:   " + runtime.maxMemory / mb)
        println("")
        memCounter = 0
      }

    }
  }

}

object Main {
  val SIZE_OF_DISPLAYED_MAP: Int = 10
  val SIZE_OF_WORDS: Int = 6
  val SIZE_OF_WINDOW: Int = 1000
  val PRINT_COUNTER: Int = 1

  def main(args: Array[String]): Unit = {
    var queueSize: Int = SIZE_OF_WINDOW
    var wordSize: Int = SIZE_OF_WORDS
    var mapSize: Int = SIZE_OF_DISPLAYED_MAP
    var printTimer: Int = PRINT_COUNTER

    if (args.length == 1) {
      if (args(0).toInt < 1) {
        throw new NumberFormatException("Please give a number greater than zero for arg 1");
      }
      mapSize = args(0).toInt
    } else if (args.length == 2) {
      if (args(0).toInt < 1) {
        throw new NumberFormatException("Please give a number greater than zero for arg 1");
      }
      mapSize = args(0).toInt

      if (args(1).toInt < 0) {
        throw new NumberFormatException("Please give a please give a number greater than 0 for arg 2");
      }
      wordSize = args(1).toInt
    } else if (args.length == 3) {
      if (args(0).toInt < 1) {
        throw new NumberFormatException("Please give a number greater than zero for arg 1");
      }
      mapSize = args(0).toInt

      if (args(1).toInt < 0) {
        throw new NumberFormatException("Please give a non-negative natural number for arg 2");
      }
      wordSize = args(1).toInt

      if (args(2).toInt < args(0).toInt) {
        throw new NumberFormatException("Value for arg 3 cannot be smaller than value for arg 1");
      }
      queueSize = args(2).toInt
    } else if (args.length == 4) {
      if (args(0).toInt < 1) {
        throw new NumberFormatException("Please give a number greater than zero for arg 1");
      }
      mapSize = args(0).toInt

      if (args(1).toInt < 0) {
        throw new NumberFormatException("Please give a non-negative natural number for arg 2");
      }
      wordSize = args(1).toInt

      if (args(2).toInt < args(0).toInt) {
        throw new NumberFormatException("Value for arg 3 cannot be smaller than value for arg 1");
      }
      queueSize = args(2).toInt

      if (args(3).toInt < 0) {
        throw new NumberFormatException("Value for arg 4 cannot be smaller than  0");
      }
      printTimer = args(3).toInt
    }

    if ("Windows" != System.getProperty("os.name")) {
      Signal.handle(new Signal("PIPE"), (sig: Signal) => System.exit(1))
    }
    val iterator = scala.io.Source.stdin.getLines
    val words = iterator.flatMap(_.split("(?U)[^\\p{Alpha}0-9']+"))

    val window: WindowMaker = new WindowMaker(mapSize, wordSize, queueSize, printTimer)
    val printToCommandLine: Output = new Output() {
      override def update(value: Queue[String]): Unit = {
        val thingMap = value.groupBy(identity).mapValues(_.size).toSeq.sortWith(_._2 > _._2).take(mapSize)

        if (value.size >= mapSize) {
          println()
          println("Map of the numbers")
          thingMap.foreach { println }
        }
      }
    }
    window.slidingWindow(words, printToCommandLine)
  }

}

//observer
trait Output {
  def update(value: Queue[String]): Unit
}
