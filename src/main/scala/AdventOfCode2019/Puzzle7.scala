package AdventOfCode2019

import scala.io.Source

case class IntCodeComputer(var program: List[Int], var pointer: Int = 0, var settings: List[Int]) {
  def executeProgram: Int = {
    val currentProgram = program.slice(pointer, program.size)
    val instruction = getInstruction(currentProgram.head)

    instruction.opCode :: currentProgram.tail match {
      case 1 :: n1 :: n2 :: resultPosition :: tail => {
        val arg1 = getArg(n1, instruction.isFirstParamImmediate, program)
        val arg2 = getArg(n2, instruction.isSecondParamImmediate, program)

        val result = program.updated(resultPosition, arg1 + arg2)

        this.program = result
        this.pointer = pointer + 4

        executeProgram
      }
      case 2 :: n1 :: n2 :: resultPosition :: tail => {
        val arg1 = getArg(n1, instruction.isFirstParamImmediate, program)
        val arg2 = getArg(n2, instruction.isSecondParamImmediate, program)

        val result = program.updated(resultPosition, arg1 * arg2)

        this.program = result
        this.pointer = pointer + 4

        executeProgram
      }
      case 3 :: resultPosition :: tail => {
        val input = settings.head

        val result = program.updated(resultPosition, input)

        this.program = result
        this.settings = settings.tail
        this.pointer = pointer + 2

        executeProgram
      }
      case 4 :: printPosition :: tail => {
        val arg1 = getArg(printPosition, instruction.isFirstParamImmediate, program)

        this.pointer = pointer + 2

        arg1
      }
      case 5 :: n1 :: n2 :: tail => {
        val arg1 = getArg(n1, instruction.isFirstParamImmediate, program)
        val arg2 = getArg(n2, instruction.isSecondParamImmediate, program)


        if (arg1 != 0) this.pointer = arg2
        else this.pointer = this.pointer + 3

        executeProgram
      }
      case 6 :: n1 :: n2 :: tail => {
        val arg1 = getArg(n1, instruction.isFirstParamImmediate, program)
        val arg2 = getArg(n2, instruction.isSecondParamImmediate, program)

        if (arg1 == 0)
          this.pointer = arg2
        else
          this.pointer = this.pointer + 3

        executeProgram
      }
      case 7 :: n1 :: n2 :: resultPosition :: tail => {
        val arg1 = getArg(n1, instruction.isFirstParamImmediate, program)
        val arg2 = getArg(n2, instruction.isSecondParamImmediate, program)

        if (arg1 < arg2) {
          this.program = program.updated(resultPosition, 1)
          this.pointer = this.pointer + 4
        } else {
          this .program = program.updated(resultPosition, 0)
          this.pointer = this.pointer + 4
        }

        executeProgram
      }
      case 8 :: n1 :: n2 :: resultPosition :: tail => {
        val arg1 = getArg(n1, instruction.isFirstParamImmediate, program)
        val arg2 = getArg(n2, instruction.isSecondParamImmediate, program)

        if (arg1 == arg2) {
          this.program = program.updated(resultPosition, 1)
          this.pointer = this.pointer + 4
        } else {
          this.program = program.updated(resultPosition, 0)
          this.pointer = this.pointer + 4
        }

        executeProgram
      }
      case 99 :: _ => throw new Exception("Finished")
      case _ => throw new Exception("Wrong opcode")
    }
  }

  private def getInstruction(instructionCode: Int): Instruction = {
    val opCode = instructionCode % 100
    val firstParamMode = instructionCode / 100 % 10
    val secondParamMode = instructionCode / 1000 % 10
    val thirdParamMode = instructionCode / 10000 % 10

    Instruction(opCode, firstParamMode == 1, secondParamMode == 1, thirdParamMode == 1)
  }

  private def getArg(value: Int, isImmediateMode: Boolean, program: List[Int]): Int = {
    if (isImmediateMode) value
    else program(value)
  }
}

case class Circuit(amplifiers: List[Amplifier])

case class Amplifier(phaseSetting: Int, program: List[Int]) {
  val computer: IntCodeComputer = IntCodeComputer(program, 0, List(phaseSetting))
}

object Puzzle7 extends App {
  def runAmplifier(amplifier: Amplifier, input: Int): Int = {
    amplifier.computer.settings = amplifier.computer.settings :+ input
    val result = amplifier.computer.executeProgram

    result
  }

  def runCircuit(circuit: Circuit, input: Int = 0): Int = {
    val result = circuit.amplifiers.foldLeft(input) { (input, amplifier) =>
      runAmplifier(amplifier, input)
    }

    result
  }

  def runCircuitWithFeedbackLoop(circuit: Circuit, input: Int = 0): Int = {
    try {
      val result = circuit.amplifiers.foldLeft(input) { (input, amplifier) =>
        runAmplifier(amplifier, input)
      }

      runCircuitWithFeedbackLoop(circuit, result)
    } catch {
      case e: Exception => input
    }
  }

  val input = Source.fromResource("input_ex7_2").mkString.split(",")

  val program = input.map(_.toInt).toList

//  val part1Results = for {
//    set1 <- 0 to 4; set2 <- 0 to 4; set3 <- 0 to 4; set4 <- 0 to 4; set5 <- 0 to 4
//    phaseSettings = List(set1, set2, set3, set4, set5)
//    if phaseSettings.distinct.length == 5
//    circuit = Circuit(
//      List(
//        Amplifier(set1, program),
//        Amplifier(set2, program),
//        Amplifier(set3, program),
//        Amplifier(set4, program),
//        Amplifier(set5, program)
//      )
//    )
//  } yield runCircuit(circuit)
//
//  println(part1Results.max)

  val part2Results = for {
    set1 <- 5 to 9; set2 <- 5 to 9; set3 <- 5 to 9; set4 <- 5 to 9; set5 <- 5 to 9
    phaseSettings = List(set1, set2, set3, set4, set5)
    if phaseSettings.distinct.length == 5
    circuit = Circuit(
      List(
        Amplifier(set1, program),
        Amplifier(set2, program),
        Amplifier(set3, program),
        Amplifier(set4, program),
        Amplifier(set5, program)
      )
    )
  } yield runCircuitWithFeedbackLoop(circuit)

  println(part2Results.max)
}
