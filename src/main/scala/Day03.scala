import scala.io.Source

object Day03 extends App {
  val filename = "input_03" //
  val data = Source.fromFile(filename).getLines.toList

  val r  =data.map(_.split("").map(_.toInt)).reduce( (a,b) => {
    a.zip(b).map( c => c._1+c._2)

  })
    .map(a => if(a >= data.length/2) 1 else 0 )
    .reverse
    .zipWithIndex
    .foldLeft(0)( (acc,el) =>
      acc + (el._1 << el._2)
    )
  println(r.toBinaryString)
  val s =( (Math.pow(2,r.toBinaryString.length).toInt -1) ^ r )

  println(r*s)
}
