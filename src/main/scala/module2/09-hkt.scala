package module2

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _))}

  trait Combiner[F[_]] {
    def combine[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Combiner {
    implicit val listCombiner: Combiner[List] = new Combiner[List] {
      def combine[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
        fa.flatMap(a => fb.map(b => (a, b)))
    }

    implicit val optionCombiner: Combiner[Option] = new Combiner[Option] {
      def combine[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
        fa.flatMap(a => fb.map(b => (a, b)))
    }

    implicit def eitherCombiner[E]: Combiner[({type lambda[A] = Either[E, A]})#lambda] = new Combiner[({type lambda[A] = Either[E, A]})#lambda] {
      def combine[A, B](fa: Either[E, A], fb: Either[E, B]): Either[E, (A, B)] =
        fa.flatMap(a => fb.map(b => (a, b)))
    }
  }

  def tuplef[F[_]: Combiner, A, B](fa: F[A], fb: F[B]): F[(A, B)] = implicitly[Combiner[F]].combine(fa, fb)


  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
    override def map[B](f: A => B): Option[B] = opt.map(f)

    override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
  }

  def listBindable[A](list: List[A]): Bindable[List, A] = ???




  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r3: Option[(Int, Int)] = tupleBindable(optBindable(optA), optBindable(optB))
  val r4 = println(tupleBindable(listBindable(list1), listBindable(list2)))


  lazy val r1 = println(tuplef(optA, optB))
  lazy val r2 = println(tuplef(list1, list2))

}