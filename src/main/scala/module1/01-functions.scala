package module1

object functions {


  /**
   * Функции
   */



  /**
   * Реализовать ф-цию  sum, которая будет суммировать 2 целых числа и выдавать результат
   */

   def sum(x: Int, y: Int): Int = x + y

  val r1: Int = sum(2, 3) // 5

  val sum2: Function2[Int, Int, Int] = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }

  val r2: Int = sum2(2, 3) // 5

  val sum3: (Int, Int) => Int = sum _

  sum3(2, 3) // 5

  List(sum2, sum3)



  // Partial function

  val divide: PartialFunction[(Int, Int), Int] = new PartialFunction[(Int, Int), Int] {
    override def isDefinedAt(x: (Int, Int)): Boolean = x._2 != 0

    override def apply(v1: (Int, Int)): Int = v1._1 / v1._2
  }

  val divide2: PartialFunction[(Int, Int), Int] = {
    case (a, b) if b != 0 => a / b
  }


  divide2.isDefinedAt(5, 0) // false
  divide2.isDefinedAt(4, 2) // true
  divide2(4, 2)

  val r5 = List((1, 1), (2, 2), (4, 2), (5, 0)).collect(divide)


  // SAM Single Abstract Method

  trait Printer {
    def apply(s: String): Unit
  }

  val p: Printer = a => println(a)

  p("hello world")

  /**
   *  Задание 1. Написать ф-цию метод isEven, которая будет вычислять является ли число четным
   */

  def isEven(num: Int): Boolean = if (num % 2 == 0) true else false

  /**
   * Задание 2. Написать ф-цию метод isOdd, которая будет вычислять является ли число нечетным
   */

  def isOdd(num: Int): Boolean = if (!isEven(num)) true else false

  /**
   * Задание 3. Написать ф-цию метод filterEven, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются четными
   */


  def filterEven(arr: Array[Int]): Array[Int] = arr.filter(x => x % 2 == 0)

  /**
   * Задание 4. Написать ф-цию метод filterOdd, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются нечетными
   */

  def filterOdd(arr: Array[Int]): Array[Int] = arr.filter(x => !(x % 2 == 0))

  /**
   * return statement
   *
   */
}