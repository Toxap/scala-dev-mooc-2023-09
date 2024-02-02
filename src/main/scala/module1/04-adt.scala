package module1

import java.time.LocalDate
import java.time.YearMonth

object adt {

  object tuples {

    /** Tuples ()
      */
    type ProductUnitBoolean = (Unit, Boolean)


    /** Создать возможные экземпляры с типом ProductUnitBoolean
      */

    lazy val p1: ProductUnitBoolean = ((), true)
    lazy val p2: ProductUnitBoolean = ((), false)


    /** Реализовать тип Person который будет содержать имя и возраст
      */

    type Person = (String, Int)


    /**  Реализовать тип `CreditCard` который может содержать номер (String),
      *  дату окончания (java.time.YearMonth), имя (String), код безопастности (Short)
      */

    type CreditCard = (String, java.time.YearMonth, String, Short)

    lazy val cc: CreditCard = ???


  }

  object case_classes {

    /** Case classes
      */


    //  Реализовать Person с помощью case класса
    case class Person(name: String, age: Int)


    // Создать экземпляр для Tony Stark 42 года

    val tony: Person = ???

    // Создать case class для кредитной карты

    case class CreditCard(number: String, date: java.time.YearMonth, name: String, cvc: Short)

  }



  object either {

    /** Sum
      */

    /** Either - это наиболее общий способ хранить один из двух или более кусочков информации в одно время.
      * Также как и кортежи обладает целым рядом полезных методов
      * Иммутабелен
      */

    // trait Ether
    // Right() extends Either
    // Left() extends Either

    class Foo
    class Bar extends Foo
    class Bar2 extends Foo

    type IntOrString = Either[Int, String]
    type BooleanOrUnit = Either[Boolean, Bar]

    val v1: BooleanOrUnit = Right(new Bar)
    val v2: BooleanOrUnit = Left(true)
    val v3: BooleanOrUnit = Left(false)

    /** Реализовать экземпляр типа IntOrString с помощью конструктора Right
      */
   // val intOrString = Either[Int, String]


    type CreditCard
    type WireTransfer
    type Cash

    /** \
      * Реализовать тип PaymentMethod который может быть представлен одной из альтернатив
      */
    type PaymentMethod = Either[CreditCard, Either[WireTransfer, Cash]]

   // Left[CreditCard]
   // Right[Left[WireTransfer]]
   //  Right[Right[Cash]]

  }

  object sealed_traits {

    /** Также Sum type можно представить в виде sealed trait с набором альтернатив
      */

    sealed trait PaymentMethod
    case object CreditCard extends PaymentMethod
    case object WireTransfer extends PaymentMethod
    case object Cash extends PaymentMethod


  }

  object cards {

    sealed trait Suit                 // масть
    case object Clubs extends Suit               // крести
    case object Diamonds extends Suit             // бубны
    case object Spades extends Suit               // пики
    case object Hearts extends Suit               // червы
    sealed trait Rank                      // номинал
    case object Two     extends Rank            // двойка
    case object Three   extends Rank            // тройка
    case object Four    extends Rank            // четверка
    case object Five    extends Rank            // пятерка
    case object Six     extends Rank            // шестерка
    case object Seven   extends Rank            // семерка
    case object Eight   extends Rank            // восьмерка
    case object Nine    extends Rank            // девятка
    case object Ten     extends Rank            // десятка
    case object Jack    extends Rank            // валет
    case object Queen   extends Rank            // дама
    case object King    extends Rank            // король
    case object Ace     extends Rank            // туз
    case class Card(suit: Suit, rank: Rank)                 // карта
    type Deck = Set[Card]                 // колода
    type Hand = Set[Card]                // рука
    case class Player(name: String, hand: Hand)               // игрок
    case class Game(deck: Deck, players: Set[Player])                 // игра
    type PickupCard           // взять карту

  }

}
