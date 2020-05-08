package AdventOfCode2019

import scala.io.Source

object Puzzle1 extends App {
  val input = Source.fromResource("input_ex1").mkString.split("\n")

  val result = input
    .map(_.toInt)
    .map(getRequiredFuel)
    .sum

  println(result)

  def getRequiredFuel(mass: Int): Int = {
    val requiredFuel = (mass / 3) - 2

    if (requiredFuel <= 0) 0
    else requiredFuel + getRequiredFuel(requiredFuel)
  }
}
