package io.github.nikiforo.aoc23

import scala.collection.mutable
import scala.collection.immutable.Queue

object Day20 extends DayApp("20") {

  sealed trait Pulse
  case object High extends Pulse
  case object Low extends Pulse

  sealed trait Module {
    def destinations: List[String]
    def register(source: String): Unit
    def receive(source: String, pulse: Pulse): Option[Pulse]
  }

  final class FlipFlop(val destinations: List[String]) extends Module {

    private var isOn = false

    def register(source: String) {}

    def receive(source: String, pulse: Pulse): Option[Pulse] =
      pulse match {
        case High => None
        case Low =>
          isOn = !isOn
          if (isOn) Some(High) else Some(Low)
      }
  }

  final class Conjunction(val destinations: List[String]) extends Module {

    private val lowPulses = mutable.Set.empty[String]

    def register(source: String) { lowPulses += source }

    def receive(source: String, pulse: Pulse): Option[Pulse] = {
      pulse match {
        case High => lowPulses -= source
        case Low => lowPulses += source
      }
      Some(if (lowPulses.isEmpty) Low else High)
    }
  }

  private val Broadcaster = "broadcaster"

  def task1(lines: List[String]): Long = {
    val (broadcast, map) = parse(lines)
    initModules(map)
    val queue = Queue.from(broadcast.map((_, Broadcaster, Low: Pulse)))
    val (highs, lows) = (1 to 1000).map(_ => run(map, queue, 0, 1)).unzip
    highs.sum * lows.sum
  }

  private def initModules(map: Map[String, Module]) =
    for {
      (name, module) <- map
      destination <- module.destinations
      mod <- map.get(destination)
    } mod.register(name)

  private def run(map: Map[String, Module], queue: Queue[(String, String, Pulse)], high: Long, low: Long): (Long, Long) =
    if (queue.isEmpty) (high, low)
    else {
      val (dest, source, sentPulse) = queue.head
      val newEntries =
        for {
          module <- map.get(dest).toList
          pulse <- module.receive(source, sentPulse).toList
          destination <- module.destinations
        } yield (destination, dest, pulse)
      val nHigh = if (sentPulse == High) high + 1 else high
      val nLow = if (sentPulse == Low) low + 1 else low
      run(map, queue.tail :++ newEntries, nHigh, nLow)
    }

    // Won't solve in time
  def task2(lines: List[String]): Long = {
    val (broadcast, map) = parse(lines)
    initModules(map)
    val queue = Queue.from(broadcast.map((_, Broadcaster, Low: Pulse)))
    from(1L).find(_ => run2(map, queue)).get
  }

  private def run2(map: Map[String, Module], queue: Queue[(String, String, Pulse)]): Boolean =
    queue.headOption.fold(false) { case (dest, source, sentPulse) =>
      if (dest == "rx" && sentPulse == Low) true
      else {
        val newEntries =
          for {
            module <- map.get(dest).toList
            pulse <- module.receive(source, sentPulse).toList
            destination <- module.destinations
          } yield (destination, dest, pulse)
        run2(map, queue.tail :++ newEntries)
      }
    }

  def from[T](l: Long): LazyList[Long] = {
    if (l % 1_000_000 == 0) println(l)
    l #:: from(l + 1)
  }


  private def parse(lines: List[String]) = {
    val modules = lines.collect {
      case s"%$name -> $destinations" => name -> new FlipFlop(destinations.split(", ").toList)
      case s"&$name -> $destinations" => name -> new Conjunction(destinations.split(", ").toList)
    }
    (lines.collectFirst { case s"$Broadcaster -> $destinations" => destinations.split(", ") }.get, modules.toMap)
  }
}
