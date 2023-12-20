package io.github.nikiforo.aoc23

import scala.collection.mutable

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

  // 655382198 - too low
  // 737679780
  def task1(lines: List[String]): Long = {
    val broadcast = lines.collectFirst { case s"$Broadcaster -> $destinations" => destinations.split(", ") }.get
    val modules = lines.collect {
      case s"%$name -> $destinations" => name -> new FlipFlop(destinations.split(", ").toList)
      case s"&$name -> $destinations" => name -> new Conjunction(destinations.split(", ").toList)
    }
    val map = modules.toMap
    for {
      (name, module) <- modules
      destination <- module.destinations
      mod <- map.get(destination)
    } mod.register(name)
    val (highs, lows) = (1 to 1000).map(_ => run(map, broadcast)).unzip
    highs.sum * lows.sum
  }

  private def run(map: Map[String, Module], broadcast: Array[String]): (Long, Long) = {
    var (high, low) = (0L, 1L)
    val queue = mutable.Queue.from(broadcast.map((_, Broadcaster, Low: Pulse)))
    while (queue.nonEmpty) {
      val (dest, source, sentPulse) = queue.dequeue()
      if (sentPulse == High) high += 1 else low += 1
      for {
        module <- map.get(dest)
        pulse <- module.receive(source, sentPulse)
        destination <- module.destinations
      } queue.enqueue((destination, dest, pulse))
    }
    (high, low)
  }

  def task2(lines: List[String]): Long = unsolved
}
