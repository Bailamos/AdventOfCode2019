package AdventOfCode2019

import scala.io.Source
import scala.io.StdIn

case class Instruction(opCode: Int, isFirstParamImmediate: Boolean, isSecondParamImmediate: Boolean, isThirdParamImmediate: Boolean)

object Puzzle5 extends App {
  val inputPart1 = Source.fromResource("input_ex5").mkString.split(",")
  val inputPart2 = Source.fromResource("input_ex5_2").mkString.split(",")

  val programPart1 = inputPart1.map(_.toInt).toList
  val programPart2 = inputPart2.map(_.toInt).toList

  executeProgram(programPart1)
  executeProgram(programPart2)

  def executeProgram(program: List[Int], pointer: Int = 0): List[Int] = {
    val currentProgram = program.slice(pointer, program.size)
    val instruction = getInstruction(currentProgram.head)

    instruction.opCode :: currentProgram.tail match {
      case 1 :: n1 :: n2 :: resultPosition :: tail => {
        val arg1 = getArg(n1, instruction.isFirstParamImmediate, program)
        val arg2 = getArg(n2, instruction.isSecondParamImmediate, program)

        val result = program.updated(resultPosition, arg1 + arg2)

        executeProgram(result, pointer + 4)
      }
      case 2 :: n1 :: n2 :: resultPosition :: tail => {
        val arg1 = getArg(n1, instruction.isFirstParamImmediate, program)
        val arg2 = getArg(n2, instruction.isSecondParamImmediate, program)

        val result = program.updated(resultPosition, arg1 * arg2)

        executeProgram(result, pointer + 4)
      }
      case 3 :: resultPosition :: tail => {
        val input = StdIn.readInt()

        val result = program.updated(resultPosition, input)

        executeProgram(result, pointer + 2)
      }
      case 4 :: printPosition :: tail => {
        val arg1 = getArg(printPosition, instruction.isFirstParamImmediate, program)
        println(arg1)

        executeProgram(program, pointer + 2)
      }
      case 5 :: n1 :: n2 :: tail => {
        val arg1 = getArg(n1, instruction.isFirstParamImmediate, program)
        val arg2 = getArg(n2, instruction.isSecondParamImmediate, program)

        if (arg1 != 0) executeProgram(program, arg2)
        else executeProgram(program, pointer + 3)
      }
      case 6 :: n1 :: n2 :: tail => {
        val arg1 = getArg(n1, instruction.isFirstParamImmediate, program)
        val arg2 = getArg(n2, instruction.isSecondParamImmediate, program)

        if (arg1 == 0) executeProgram(program, arg2)
        else executeProgram(program, pointer + 3)
      }
      case 7 :: n1 :: n2 :: resultPosition :: tail => {
        val arg1 = getArg(n1, instruction.isFirstParamImmediate, program)
        val arg2 = getArg(n2, instruction.isSecondParamImmediate, program)

        if (arg1 < arg2) executeProgram(program.updated(resultPosition, 1), pointer + 4)
        else executeProgram(program.updated(resultPosition, 0), pointer + 4)
      }
      case 8 :: n1 :: n2 :: resultPosition :: tail => {
        val arg1 = getArg(n1, instruction.isFirstParamImmediate, program)
        val arg2 = getArg(n2, instruction.isSecondParamImmediate, program)

        if (arg1 == arg2) executeProgram(program.updated(resultPosition, 1), pointer + 4)
        else executeProgram(program.updated(resultPosition, 0), pointer + 4)
      }
      case 99 :: tail => program
      case _ => throw new Exception("Wrong opcode")
    }
  }

  def getInstruction(instructionCode: Int): Instruction = {
    val opCode = instructionCode % 100
    val firstParamMode = instructionCode / 100 % 10
    val secondParamMode = instructionCode / 1000 % 10
    val thirdParamMode = instructionCode / 10000 % 10

    Instruction(opCode, firstParamMode == 1, secondParamMode == 1, thirdParamMode == 1)
  }

  def getArg(value: Int, isImmediateMode: Boolean, program: List[Int]): Int = {
    if (isImmediateMode) value
    else program(value)
  }
}
