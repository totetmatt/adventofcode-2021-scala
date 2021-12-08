import scala.io.Source
case class Vec2(x: Int, y: Int)
object Day05 extends App {
  val filename = "input_05" //
  val data = Source.fromFile(filename).getLines.toList
  def gengen(
      from: Vec2,
      to: Vec2,
      dir: Vec2,
      result: Seq[Vec2] = Seq.empty
  ): Seq[Vec2] = {
    val r = from +: result
    if (from == to) {
      r
    } else {
      gengen(from.copy(from.x + dir.x, from.y + dir.y), to, dir, r)
    }

  }
  def gen(from: Vec2, to: Vec2) = {

    if (from.x == to.x) {

      Some(
        Math
          .min(from.y, to.y)
          .to(Math.max(from.y, to.y))
          .map(i => Vec2(from.x, i))
      )
    } else if (from.y == to.y) {
      Some(
        Math
          .min(from.x, to.x)
          .to(Math.max(from.x, to.x))
          .map(i => Vec2(i, from.y))
      )
    } else {

      Some(
        Iterator.from(
          gengen(
            from,
            to,
            Vec2(
              Math.signum(to.x - from.x).toInt,
              Math.signum(to.y - from.y).toInt
            )
          )
        )
      )
    }
  }
  val result = data
    .flatMap(line => {
      val Array(from, to) = line.split(" -> ")
      val Array(fromX, fromY) = from.split(',').map(_.strip.toInt)
      val Array(toX, toY) = to.split(',').map(_.strip.toInt)
      gen(
        Vec2(fromX, fromY),
        Vec2(toX, toY)
      )
    })
    .flatten
    .groupBy(identity)
    .count(_._2.size > 1)
  println(result)

}
