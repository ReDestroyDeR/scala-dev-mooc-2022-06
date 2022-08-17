package module1
import list.{List, incList, shoutString}

object App {
  def main(args: Array[String]): Unit = {
//    def sumItUp: Int = {
//      def one(x: Int): Int = { return x; 1 }
//      val two = (x: Int) => { return x; 2 }
//      1 + one(2) + two(11)
//    }
//
//    println(sumItUp)
//
//    println("Hello world")

    val l = List.::(5, List.Nil).::(4).::(3).::(2).::(1)
    val l2 = List.apply(5, 4, 3, 2, 1)
    println(l.mkString())
    println(l2.mkString("; "))
    println(l2.reverse().mkString("; "))
    println(l2.map(i => s"$i bananas!").mkString(" "))
    println(l2.filter(i => i % 2 == 0).map(s => s"Even $s").mkString())
    println(shoutString(List("One", "Two", "Three", "Four", "Five")).mkString())
    println(incList(l2).mkString())
  }
}
