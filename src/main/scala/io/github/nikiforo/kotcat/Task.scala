package io.github.nikiforo.kotcat

import cats.syntax.all._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait Task {
  def safeSequence[V](futures: List[Future[V]]): Future[(List[Throwable], List[V])]
}

object Recursion extends Task {

  def safeSequence[V](futures: List[Future[V]]): Future[(List[Throwable], List[V])] =
    go(futures, Future.successful(initState))

  private def go[V](
    futures: List[Future[V]],
    completed: Future[(List[Throwable], List[V])],
  ): Future[(List[Throwable], List[V])] =
    futures match {
      case Nil => completed
      case fut :: tail =>
        val nCompleted =
          completed.flatMap { case (failed, succeed) =>
            val handledSuccess = fut.map(v => (failed, succeed :+ v))
            handledSuccess.recover { case ex => (failed :+ ex, succeed) }
          }

        go(tail, nCompleted) // Как и в каких условиях мы вызываем следующий шаг итерации
    }

  private def initState[V] = (List.empty[Throwable], List.empty[V])
}

object FoldLeft extends Task {

  def safeSequence[V](futures: List[Future[V]]): Future[(List[Throwable], List[V])] =
    futures.foldLeft(Future.successful(initState[V])) { (completed, fut) =>
      completed.flatMap { case (failed, succeed) =>
        val handledSuccess = fut.map(v => (failed, succeed :+ v))
        handledSuccess.recover { case ex => (failed :+ ex, succeed) }
      }
    }

  private def initState[V] = (List.empty[Throwable], List.empty[V])
}

object FoldLeftM extends Task {

  def safeSequence[V](futures: List[Future[V]]): Future[(List[Throwable], List[V])] =
    futures.foldLeftM(initState[V]) { case ((failed, succeed), fut) =>
      val handledSuccess = fut.map(v => (failed, succeed :+ v))
      handledSuccess.recover { case ex => (failed :+ ex, succeed) }
    }

  private def initState[V] = (List.empty[Throwable], List.empty[V])
}

object FoldMapM extends Task {

  def safeSequence[V](futures: List[Future[V]]): Future[(List[Throwable], List[V])] =
    futures.foldMapM { fut =>
      val handledSuccess = fut.map(v => (List.empty, List(v)))
      handledSuccess.recover { case ex => (List(ex), List.empty) }
    }
}

object FoldMapMEither extends Task {

  def safeSequence[V](futures: List[Future[V]]): Future[(List[Throwable], List[V])] =
    futures.foldMapM { fut =>
      fut.attempt.map {
        case Right(v) => (List.empty, List(v))
        case Left(ex) => (List(ex), List.empty)
      }
    }
}

object PartitionEitherM extends Task {

  def safeSequence[V](futures: List[Future[V]]): Future[(List[Throwable], List[V])] =
    futures.partitionEitherM(_.attempt)
}
