package module1

object pattern_matching{
     // Pattern matching

  /**
   * Матчинг на типы
   */

   val i: Any = ???

   i match {
     case v: Int => println(s"Int $v")
     case v: String => println("String")
     case v: List[Int] => println("List[Int]")
     case v: List[String] => println("List[String]")
     case _ => println("Nothing matched")
   }


  /**
   * Структурный матчинг
   */

  // isInstanceOf
  // asInstanceOf


  sealed trait Animal{
    def name: String
    def age: Int

    def whoIam = this match {
      case Dog(n, _) => s" I'm dog $n"
      case Cat(n, _) => s" I'm cat $n"
    }
  }


  case class Dog(name: String, age: Int) extends Animal
  case class Cat(name: String, age: Int) extends Animal


  /**
   * Матчинг на литерал
   */

  val dog: Animal = ???

  val Bim = "Bim"

  dog match {
    case Dog("Bim", age) => ???
    case Cat(name, age) => ???
  }


  /**
   * Матчинг на константу
   */
  dog match {
    case Dog(_, age) => println(age)
    case Cat(name, age) => ???
  }


  /**
   * Матчинг с условием (гарды)
   */

  dog match {
    case Dog(name, age) if age > 5 => println(name)
    case Cat(name, age) => ???
    case _ =>
  }


  /**
   * "as" паттерн
   */

  def treatCat(cat: Cat) = ???
  def treatDog(dog: Dog) = ???



  /**
   * используя паттерн матчинг напечатать имя и возраст
   */

  def treatAnimal(a: Animal) = a match {
    case d @ Dog(n, a) => treatDog(d)
    case c @ Cat(name, age) => treatCat(c)
  }



  final case class Employee(name: String, address: Address)
  final class Address(val street: String, val number: Int)

  object Address{
    def apply(s: String, n: Int): Address = new Address(s, n)
    def unapply(a: Address): Option[(String, Int)] = Some((a.street, a.number))
  }

  val alex = Employee("Alex", Address("XXX", 221))

  /**
   * воспользовавшись паттерн матчингом напечатать номер из поля адрес
   */

   alex match {
     case Employee(_, Address(_, n)) => println(n)
   }



  /**
   * Паттерн матчинг может содержать литералы.
   * Реализовать паттерн матчинг на alex с двумя кейсами.
   * 1. Имя должно соотвествовать Alex
   * 2. Все остальные
   */




  /**
   * Паттерны могут содержать условия. В этом случае case сработает,
   * если и паттерн совпал и условие true.
   * Условия в паттерн матчинге называются гардами.
   */



  /**
   * Реализовать паттерн матчинг на alex с двумя кейсами.
   * 1. Имя должно начинаться с A
   * 2. Все остальные
   */


  /**
   *
   * Мы можем поместить кусок паттерна в переменную использую `as` паттерн,
   * x @ ..., где x это любая переменная.
   * Это переменная может использоваться, как в условии,
   * так и внутри кейса
   */

    trait PaymentMethod
    case object Card extends PaymentMethod
    case object WireTransfer extends PaymentMethod
    case object Cash extends PaymentMethod

    case class Order(paymentMethod: PaymentMethod)

    lazy val order: Order = ???

    lazy val pm: PaymentMethod = ???


    def checkByCard(o: Order) = ???

    def checkOther(o: Order) = ???



  /**
   * Мы можем использовать вертикальную черту `|` для матчинга на альтернативы
   */

   sealed trait A
   case class B(v: Int) extends A
   case class C(v: Int) extends A
   case class D(v: Int) extends A

   val a: A = ???

  a match {
    case B(_) | C(_) => println("B | C")
    case D(v) => println("D")
  }

}