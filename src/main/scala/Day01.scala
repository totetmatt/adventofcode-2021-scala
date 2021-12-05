import scala.collection.immutable.ArraySeq
import scala.io.Source

object Day01 extends App {
  val filename = "input_01.txt"
  val data = Source.fromFile(filename)
    .getLines
    .map(_.toInt).toList
  def dcount(l :List[Int]) = l.sliding(2).count(a => a.head < a.last )
  println(dcount(data))
  println(dcount(data.sliding(3).map(_.sum).toList))
}
