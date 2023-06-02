package io.github.nikiforo.photopublish

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.parallel._
import fs2.io.file.CopyFlags
import fs2.io.file.Files
import fs2.io.file.Path

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.duration._
import scala.concurrent.duration.FiniteDuration

object ParseApp extends IOApp {

  private val fio = Files[IO]

  def run(args: List[String]): IO[ExitCode] =
    args match {
      case List(dir) =>
        val path = Path(dir)
        val skew = new Skewer(-1.hour, Duration.Zero)
        Files[IO].list(path).evalFilterNot(fio.isDirectory).compile.toList.flatMap { images =>
          images.parTraverse(i => skew(i.fileName.toString).tupleRight(i)) match {
            case Left(errors) => IO.raiseError(new IllegalArgumentException(errors.toString()))
            case Right(images) => images.traverse_(moveImage(_, path)).as(ExitCode.Success)
          }
        }
      case other => IO.println(s"Unexpected $other").as(ExitCode.Error)
    }

  private def moveImage(pair: (String, Path), dir: Path) =
    fio.move(pair._2, dir / pair._1)
}

private final class Skewer(sphere: FiniteDuration, sony: FiniteDuration) {

  private val SphereRegex = "IMG_(.{15}).JPG".r
  private val RedmiRegex = "IMG_(.{15}).jpg".r
  private val RedmiNumRegex = "IMG_(.{15})_(.).jpg".r
  private val GalaxyRegex = "(.{15}).jpg".r
  private val GalaxyNumRegex = "(.{15})\\((.)\\).jpg".r
  private val SonyRegex = "(.{17})-DSC(.....).[jJ][pP][gG]".r

  val formatter15 = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss")
  val formatter17 = DateTimeFormatter.ofPattern("yyyy-MM-dd_HHmmss")

  def apply(name: String): Either[List[String], String] =
    name match {
      case SphereRegex(date) => formatSphere(date).asRight
      case RedmiRegex(date) => formatRedmi(date, "").asRight
      case RedmiNumRegex(date, num) => formatRedmi(date, s"-$num").asRight
      case GalaxyRegex(date) => formatGalaxy(date, "").asRight
      case GalaxyNumRegex(date, num) => formatGalaxy(date, s"-$num").asRight
      case SonyRegex(date, dsc) => formatSony(date, dsc).asRight
      case other => List(other).asLeft
    }

  private def formatSphere(date: String) = {
    val time = LocalDateTime.parse(date, formatter15).plusSeconds(sphere.toSeconds)
    s"${formatter15.format(time)}-sphere.JPG"
  }

  private def formatRedmi(date: String, extra: String): String = {
    val time = LocalDateTime.parse(date, formatter15)
    s"${formatter15.format(time)}$extra-redmi.jpg"
  }

  private def formatGalaxy(date: String, extra: String): String = {
    val time = LocalDateTime.parse(date, formatter15)
    s"${formatter15.format(time)}$extra-galaxy.jpg"
  }

  private def formatSony(date: String, dsc: String) = {
    val time = LocalDateTime.parse(date, formatter17).plusSeconds(sony.toSeconds)
    s"${formatter15.format(time)}-DSC$dsc.JPG"
  }
}
