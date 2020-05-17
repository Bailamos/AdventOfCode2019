package AdventOfCode2019


abstract class Rule {
  def apply(number: Int): Boolean = validate(number)

  protected def validate(number: Int): Boolean
}

object IsSixDigit extends Rule {
  override def validate(number: Int): Boolean = {
    number.toString.length == 6
  }
}

object AtLeastTwoAdjacentDigitsTheSame extends Rule {
  override def validate(number: Int): Boolean = validate(number.toString.toList)

  private def validate(number: List[Char]): Boolean = number match {
    case x :: y :: tail => if (x == y) true else validate(y :: tail)
    case x :: Nil => false
  }
}

object DigitsNeverDecrease extends Rule {
  override def validate(number: Int): Boolean = validate(number.toString.toList)

  private def validate(number: List[Char]): Boolean = number match {
    case x :: y :: tail => if (x > y) false else validate(y :: tail)
    case x :: Nil => true
  }
}

object AtLeastTwoAdjacentDigitsTheSameAndNotPartOfLargerGroup extends Rule {
  override def validate(number: Int): Boolean = validate(number.toString.toList)

  private def validate(number: List[Char]): Boolean = {
    if (number.isEmpty) return false

    val head = number.head
    val numberOfSameAdjacentDigits = number.takeWhile(_ == head).length

    if (numberOfSameAdjacentDigits == 2) true
    else validate(number.drop(numberOfSameAdjacentDigits))
  }
}

object Puzzle4 extends App {
  val input = 246515 to 739105

  val resultPart1 = input.count { n =>
      IsSixDigit(n) &&
      AtLeastTwoAdjacentDigitsTheSame(n) &&
      DigitsNeverDecrease(n)
  }
  println(resultPart1)

  val resultPart2 = input.count { n =>
      IsSixDigit(n) &&
      AtLeastTwoAdjacentDigitsTheSameAndNotPartOfLargerGroup(n) &&
      DigitsNeverDecrease(n)
  }
  println(resultPart2)
}
