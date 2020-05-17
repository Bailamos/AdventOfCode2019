package AdventOfCode2019

import org.scalatest.{FlatSpec, Matchers}

class Puzzle4Test extends FlatSpec with Matchers{

  "AtLeastTwoAdjacentDigitsTheSame validate" should "return true when there are two same adjacent digits" in {
    val input = 1234566

    val result = AtLeastTwoAdjacentDigitsTheSame(input)

    result shouldBe true
  }
  it should "return false when there are not two same adajcent digits" in {
    val input = 123456

    val result = AtLeastTwoAdjacentDigitsTheSame(input)

    result shouldBe false
  }

  "DigitsNeverDecrease validate" should "return true when following digits are ascending" in {
    val input = 1234566

    val result = DigitsNeverDecrease(input)

    result shouldBe true
  }
  it should "return false when digits decrease" in {
    val input = 1234565

    val result = DigitsNeverDecrease(input)

    result shouldBe false
  }

  "AtLeastTwoAdjacentDigitsTheSameAndNotPartOfLargerGroup validate" should "return true when there are two same adjacent digits" in {
    val input = 1233456

    val result = AtLeastTwoAdjacentDigitsTheSameAndNotPartOfLargerGroup(input)

    result shouldBe true
  }
  it should "return false when same adjacent digits are part of larger group" in {
    val input = 12333456

    val result = AtLeastTwoAdjacentDigitsTheSameAndNotPartOfLargerGroup(input)

    result shouldBe false
  }
  it should "return false when there are 5 same digits" in {
    val input = 33333

    val result = AtLeastTwoAdjacentDigitsTheSameAndNotPartOfLargerGroup(input)

    result shouldBe false
  }
}
