package module2

import module2.type_classes.JsValue.{JsNull, JsNumber, JsString}


object type_classes {

  sealed trait JsValue
  object JsValue {
    final case class JsObject(get: Map[String, JsValue]) extends JsValue
    final case class JsString(get: String) extends JsValue
    final case class JsNumber(get: Double) extends JsValue
    final case object JsNull extends JsValue
  }

  // 1
  trait JsonWriter[T]{
    def write(v: T): JsValue
  }

  object JsonWriter{

    def apply[T](implicit ev: JsonWriter[T]): JsonWriter[T] = ev


    def from[T](f: T => JsValue): JsonWriter[T] = v => f(v)

    implicit val strJsonWriter = from[String](JsString)

    implicit val intJsonWriter = from[Int](JsNumber(_))

    implicit def optJsonWriter[A](implicit ev: JsonWriter[A]) = from[Option[A]] {
      case Some(value) => ev.write(value)
      case None => JsNull
    }
  }

  implicit class JsonSyntax[T](v: T){
    def toJson(implicit ev: JsonWriter[T]): JsValue = ev.write(v)
  }

  def toJson[T: JsonWriter](v: T): JsValue = {
    JsonWriter[T].write(v)
  }

  toJson("bjhbjfghbgj")
  toJson(12)
  toJson(Option(21))
  toJson(Option("vfbfgb"))

  "vbfbgbbg".toJson
  12.toJson
  Option(13).toJson


  class A{
    def foo: String = ???
  }

  class B {
    def foo: String = ???
  }

  val a: A = ???
  val b: B = ???

  def bar[T <: {def foo: String}](v : T): String = ???

  bar(a)
  bar(b)

  // 1 type constructor
  trait Ordering[T]{
    def less(a: T, b: T): Boolean
  }

  object Ordering{

    def from[A](f: (A, A) => Boolean): Ordering[A] = new Ordering[A] {
      override def less(a: A, b: A): Boolean = f(a, b)
    }

    // 2 имплисит значения
    implicit val ordInt = from[Int](_ < _)

    implicit val ordString = from[String](_ < _)
  }



  // 3 имплисит параметр
  def _max[A](a: A, b: A)(implicit ordering: Ordering[A]): A =
    if(ordering.less(a, b)) b else a

  _max(5, 10) // 10
  _max("ab", "abc") // "abc"

  // 1
  trait Eq[T]{
    def ===(a: T, b: T): Boolean
  }

  object Eq{


    // 2
    implicit val strEq = new Eq[String] {
      override def ===(a: String, b: String): Boolean = a == b
    }
  }


  // 4
  implicit class EqSyntax[T](a: T){
    // 3
    def ===(b: T)(implicit ev: Eq[T]): Boolean = ev.===(a, b)
  }

  val result = List("a", "b", "c").filter(str => str === "1")


  // 3
  def tuplef[F[_] : Bindable, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    Bindable[F].flatMap(fa)(a => Bindable[F].map(fb)(b => (a, b)))


  // 1
  trait Bindable[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  object Bindable{
    def apply[F[_]](implicit ev: Bindable[F]) = ev
  }


}
