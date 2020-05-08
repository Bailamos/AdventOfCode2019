package AdventOfCode2019

import org.scalatest.{FlatSpec, Matchers}

class Puzzle2Test extends FlatSpec with Matchers{

  "testExecuteProgram" should "add two values" in {
    val input = List(1,0,0,0,99)
    val expected = List(2,0,0,0,99)

    val result = Puzzle2.executeProgram(input)

    result shouldBe expected
  }
  it should "multiply two values" in {
    val input = List(2,3,0,3,99)
    val expected = List(2,3,0,6,99)

    val result = Puzzle2.executeProgram(input)

    result shouldBe expected
  }
  it should "multiply two values and save it at the end of the program" in {
    val input = List(2,4,4,5,99,0)
    val expected = List(2,4,4,5,99,9801)

    val result = Puzzle2.executeProgram(input)

    result shouldBe expected
  }
  it should "execute multiple statements and terminate" in {
    val input = List(1,1,1,4,99,5,6,0,99)
    val expected = List(30,1,1,4,2,5,6,0,99)

    val result = Puzzle2.executeProgram(input)

    result shouldBe expected
  }
}
