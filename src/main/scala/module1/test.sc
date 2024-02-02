

def printIfAny(s: Option[Any]): Unit = {
  s match {
    case Some(v) => print(v)
    case None => print("Значения нет")
  }
}

val value = Some(123)

printIfAny(value)

def zip(fv: Option[Any], sv: Option[Any]): Option[(Any, Any)] = {
  fv.flatMap(a => sv.map(y => (a, y)))
}

val optionA: Option[Int] = None
val optionB: Option[String] = Some("Hello")

val result = zip(optionA, optionB)
println(result) // Выведет: Some((5,Hello))


