package io.github.nikiforo.sonyalpha

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2.io.file.Files
import fs2.io.file.Path

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZonedDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.concurrent.duration._
import cats.syntax.foldable._

object MoveApp extends IOApp {

  private val fio = Files[IO]

  def run(args: List[String]): IO[ExitCode] =
    args match {
      case List(dir) => doRun(Path(dir)).as(ExitCode.Success)
      case other => IO.println(s"Unexpected $other, arguments should be: dir").as(ExitCode.Error)
    }

  private def doRun(dir: Path): IO[Unit] = {
    val skew = sonyAlfaSkew
    val prefixFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd_HHmmss")
    fio.list(dir).compile.toList.flatMap { files =>
      files.traverse_ { file =>
        fio.getPosixFileAttributes(file).flatMap { attr =>
          val systemTime = Instant.EPOCH.plusNanos(attr.creationTime.toNanos).atZone(ZoneId.systemDefault())
          val time = systemTime.plusNanos(skew.toNanos)
          val prevName = file.fileName.toString
          val destFile = dir / s"${time.format(prefixFormatter)}-$prevName"
          fio.move(file, destFile)
        }
      }
    }
  }

  private def sonyAlfaSkew = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val actualTime = LocalDateTime.parse("2023-05-24 17:22:49", formatter)
    val skewedTime = LocalDateTime.parse("2021-03-07 06:36:54", formatter)
    ChronoUnit.SECONDS.between(skewedTime, actualTime).seconds
  }
}
