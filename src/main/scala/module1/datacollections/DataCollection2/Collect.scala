package module1.datacollections.DataCollection2

object Collect{
  val parseRange: PartialFunction[Any, Int] = {
    case x: Int if x > 10 => x + 1
  }
  def main(args: Array[String]): Unit = {
    List(15, 3, "aString").collect(parseRange).foreach(println)
  }
}
