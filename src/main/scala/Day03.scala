import scala.io.Source

object Day03 extends App {
  val filename = "input_03" //
  val data = Source.fromFile(filename).getLines.toList

  val mostCommon = data
    .map(_.split("").map(_.toInt))
    .reduce((a, b) => {
      a.zip(b).map(c => c._1 + c._2)

    })
    .map(a => if (a >= data.length / 2) 1 else 0)

  val gamma = mostCommon.reverse.zipWithIndex
    .foldLeft(0)((acc, el) => acc + (el._1 << el._2))
  println(gamma.toBinaryString)
  val epsilon = (Math.pow(2, gamma.toBinaryString.length).toInt - 1) ^ gamma

  println(gamma * epsilon)

  def mostCommonAt(l: Seq[String], idx: Int) = {
    l.map(_.apply(idx))
      .groupBy(identity)
      .view
      .mapValues(_.size)
  }
  def ogrMostCommonAt(l: Seq[String], idx: Int) = {
    mostCommonAt(l, idx).reduce((acc, el) =>
      if (el._2 > acc._2) el
      else if (el._2 < acc._2) acc
      else {
        if (el._1 == 1) el else acc
      }
    )
  }

  var ogr = (0 until data.head.length)
    .foldLeft(data)((acc, idx) => {
      if (acc.length == 1) acc
      else
        acc.filter(_.apply(idx) == ogrMostCommonAt(acc, idx)._1)
    })
    .head
  var csr = (0 until data.head.length)
    .foldLeft(data)((acc, idx) => {
      if (acc.length == 1) acc
      else
        acc.filter(_.apply(idx) != ogrMostCommonAt(acc, idx)._1)
    })
    .head
  println(Integer.parseInt(csr, 2) * Integer.parseInt(ogr, 2))

}
