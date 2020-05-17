package AdventOfCode2019

import scala.io.Source

abstract class Move(steps: Int)
case class Right(steps: Int) extends Move(steps)
case class Left(steps: Int) extends Move(steps)
case class Up(steps: Int) extends Move(steps)
case class Down(steps: Int) extends Move(steps)

case class Position(x: Int, y: Int)

object Puzzle3 extends App {
  val input = Source.fromResource("input_ex3").getLines()
  val firstWireInput = input.next().split(",").toList
  val secondWireInput = input.next().split(",").toList

  val firstWireMoves = firstWireInput.map(parseInput)
  val secondWireMoves = secondWireInput.map(parseInput)

  val port = Position(0, 0)
  val wire1 = getWire(port, firstWireMoves)
  val wire2 = getWire(port, secondWireMoves)

  val crossings = wire1 intersect wire2

  val resultPart1 = crossings.map(getManhattanDistance(_)).min
  println(resultPart1)

  val combinedStepsDistances = getCombinedStepsDistance(crossings, wire1, wire2)
  val resultPart2 = combinedStepsDistances.min
  println(resultPart2)

  def parseInput(input: String): Move = {
    val steps = input.tail.toInt

    input match {
      case x if x.startsWith("R") => Right(steps)
      case x if x.startsWith("L") => Left(steps)
      case x if x.startsWith("U") => Up(steps)
      case x if x.startsWith("D") => Down(steps)
    }
  }

  def getWire(port: Position, moves: List[Move]): List[Position] = {
    moves match {
      case head :: tail => {
        val segment = getWireSegment(port, head)

        segment ++ getWire(segment.last, tail)
      }
      case Nil => Nil
    }

  }

  def getWireSegment(start: Position, move: Move): List[Position] = {
    move match {
      case Right(steps) => (1 to steps).map(mv => Position(start.x + mv, start.y)).toList
      case Left(steps) => (1 to steps).map(mv => Position(start.x - mv, start.y)).toList
      case Up(steps) => (1 to steps).map(mv => Position(start.x, start.y + mv)).toList
      case Down(steps) => (1 to steps).map(mv => Position(start.x, start.y - mv)).toList
    }
  }

  def getManhattanDistance(position: Position, port: Position = Position(0, 0)): Int = {
    Math.abs(position.x - port.x) + Math.abs(position.y - port.y)
  }

  def getCombinedStepsDistance(crossings: List[Position], wire1: List[Position], wire2: List[Position]): List[Int] = {
    for {
      crossing <- crossings
      list1 = wire1.takeWhile(_ != crossing)
      list2 = wire2.takeWhile(_ != crossing)
    } yield list1.size + list2.size + 2
  }
}
