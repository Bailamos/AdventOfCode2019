package AdventOfCode2019

import org.scalatest.{FlatSpec, Matchers}

class Puzzle6Test extends FlatSpec with Matchers{

  "getOrbitsCount" should "return 0 with single planet in System" in {
    val input = System("Test", List())

    val result = Puzzle6.getOrbitsCount(input)

    result shouldBe 0
  }
  it should "return 1 with 2 planet in System connected in line" in {
    val input = System("Test", List(System("Test", List())))

    val result = Puzzle6.getOrbitsCount(input)

    result shouldBe 1
  }
  it should "return 3 with 3 planet in System connected in line" in {
    val input = System("Test", List(System("Test", List(System("Test", List())))))

    val result = Puzzle6.getOrbitsCount(input)

    result shouldBe 3
  }
  it should "return 2 for 1 planet with 2 satellites" in {
    val input = System("Test", List(System("Test", List()), System("Test", List())))

    val result = Puzzle6.getOrbitsCount(input)

    result shouldBe 2
  }
  it should "return 5 for 1 planet with 1 satellite which has 2 satellites" in {
    val input = System("Test", List(System("Test", List(System("Test", List()), System("Test", List())))))

    val result = Puzzle6.getOrbitsCount(input)

    result shouldBe 5
  }

  it should "return 18 for 1 planet with 2 systems with 1 planet with 1 satellite which has 2 satellites" in {
    val input = System(
      "Test",
      List(
        System("Test", List(
            System("Test", List(System("Test", List()), System("Test", List())))
          )
        ),
        System("Test", List(
            System("Test", List(System("Test", List()), System("Test", List())))
          )
        )
      )
    )

    val result = Puzzle6.getOrbitsCount(input)

    result shouldBe 18
  }
}
