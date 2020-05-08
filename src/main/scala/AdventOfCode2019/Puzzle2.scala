package AdventOfCode2019

import scala.io.Source

object Puzzle2 extends App {
  val input = Source.fromResource("input_ex2").mkString.split(",")
  val program = input.map(_.toInt).toList.updated(1, 12).updated(2, 2)


  val result = for {
    x <- 1 to 100
    y <- 1 to 100
    updatedProgram = program.updated(1, x).updated(2, y)
    result = executeProgram(updatedProgram)
    if result.head == 19690720
  } yield 100 * x + y

  println(result)

  def executeProgram(program: List[Int], pointer: Int = 0): List[Int] = {
    program.slice(pointer, program.size) match {
      case 1 :: n1 :: n2 :: resultPosition :: tail => {
        val result = program.updated(resultPosition, program(n1) + program(n2))

        executeProgram(result, pointer + 4)
      }
      case 2 :: n1 :: n2 :: resultPosition :: tail => {
        val result = program.updated(resultPosition, program(n1) * program(n2))

        executeProgram(result, pointer + 4)
      }
      case 99 :: tail => program
    }
  }
}
