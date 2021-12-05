import scala.io.Source

object Day02 extends App {
  val filename = "input_02.txt"
  val data = Source.fromFile(filename)
    .getLines
    .map(_.split(' '))
    .map( x=> (x(0),x(1).toInt) )
    .foldLeft((0,0,0))( (acc,el) => {
       el._1 match {
         case "forward" => acc.copy(_1=acc._1+el._2,_2=acc._2+(acc._3*el._2))
         case "up"  => acc.copy(_3=acc._3-el._2)
         case "down" => acc.copy(_3=acc._3+el._2)
       }
    })

  println(data)
  println(data._1 * data._2)
}
