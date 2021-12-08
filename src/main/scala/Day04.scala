import scala.io.Source
case class Coord(line: Int, col: Int)
case class Cell(number: Int, activate: Boolean)
case class Board(b: Map[Coord, Cell]) {
  def mark(c: Coord): Board = {
    Board(b.updated(c, b(c).copy(activate = true)))
  }
  def isWinning: Boolean =
    (0 until 5).exists(id => {
      println(
        b.view.filterKeys(_.col == id).map(_._2.activate).forall(identity)
      )
      b.view.filterKeys(_.col == id).map(_._2.activate).forall(identity) ||
      b.view.filterKeys(_.line == id).map(_._2.activate).forall(identity)
    })

}

object Day04 extends App {
  val filename = "input_04_ex" //
  val data = Source.fromFile(filename).getLines.toList

  val num = data.head.split(",").map(_.toInt)

  data.tail
    .grouped(6)
    .map(_.tail.init)
    .map(b => {

      Board(b.zipWithIndex.flatMap { case (l, y) =>
        l.grouped(3)
          .map(_.strip().toInt)
          .zipWithIndex
          .map { case (c, x) => Coord(x, y) -> Cell(c, false) }
      }.toMap)
    })
    .map(_.isWinning)
    .foreach(println)

  // num.foreach(println)
}
