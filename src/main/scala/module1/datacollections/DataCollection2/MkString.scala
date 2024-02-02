package module1.datacollections.DataCollection2


object MkString {
  def main(args: Array[String]): Unit = {
    val a = Array("apple", "banana", "cherry")
    println(a.mkString(";"))
  }
}
