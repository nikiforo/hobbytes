package io.github.nikiforo.kotcat

import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import org.scalatest.DoNotDiscover

@DoNotDiscover
final class TaskSuite extends AnyFunSuite {

  test("Recursion") {
    assertSafeSeq(Recursion)
  }

  test("FoldLeft") {
    assertSafeSeq(FoldLeft)
  }

  test("FoldLeftM") {
    assertSafeSeq(FoldLeftM)
  }

  test("FoldMapM") {
    assertSafeSeq(FoldMapM)
  }

  test("FoldMapMEither") {
    assertSafeSeq(FoldMapMEither)
  }

  test("PartitionEitherM") {
    assertSafeSeq(PartitionEitherM)
  }

  private val ex1 = new RuntimeException("error 1")

  private val ex2 = new RuntimeException("error 2")

  private def assertSafeSeq(task: Task) = {
    def arguments =
      List(
        Future(1),
        Future(throw ex1),
        Future(2),
        Future(throw ex2),
      )

    val actual = Await.result(task.safeSequence(arguments), 1.second)
    assert(actual == (List(ex1, ex2), List(1, 2)))
  }
}
