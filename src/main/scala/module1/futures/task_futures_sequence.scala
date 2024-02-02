package module1.futures

import module1.futures.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])(implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val processedFutures: List[Future[Either[Throwable, A]]] = futures.map { future =>
      future.map(Right(_)).recover { case ex => Left(ex) }
    }

    Future.traverse(processedFutures)(identity).map { eitherList =>
      eitherList.foldLeft((List.empty[A], List.empty[Throwable])) { (acc, either) =>
        either match {
          case Right(value) => (value :: acc._1, acc._2)
          case Left(error) => (acc._1, error :: acc._2)
        }
      }
    }.map { case (successes, failures) =>
      (successes.reverse, failures.reverse)
    }
  }





}
