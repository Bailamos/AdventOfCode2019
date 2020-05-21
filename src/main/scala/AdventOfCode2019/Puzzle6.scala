package AdventOfCode2019

import scala.io.Source

case class System(body: String, satellites: List[System]) {
  def hasSatellites(systems: List[System]): System = {
    this.copy(
      satellites = this.satellites ++ systems,
    )
  }
}

object Puzzle6 extends App {
  val input = Source.fromResource("input_ex6").getLines()
  implicit val data = parseInput(input).toList

  val center = System("COM", List())

  val system = buildSystem(center)
  val part1Result = getOrbitsCount(system)
  println(part1Result)

  val part2Result = getAllOrbitalTransfersDistancesBetweenTwoSystems("YOU", "SAN", system).flatten.min
  println(part2Result)

  def getAllOrbitalTransfersDistancesBetweenTwoSystems(from: String, to: String, system: System): List[Option[Int]] = {
    val distanceFromCurrentSystem = getOrbitalTransferDistanceThroughSystem(from, to , system)
    val distancesFromSubSystems = for {
      satellite <- system.satellites
      subSystemDistances <- getAllOrbitalTransfersDistancesBetweenTwoSystems(from, to, satellite)
    } yield subSystemDistances

    List(distanceFromCurrentSystem) ++ distancesFromSubSystems
  }

  def getOrbitalTransferDistanceThroughSystem(from: String, to: String, system: System): Option[Int] = {
    val res1 = getDistanceBetweenSystems(system, from)
    val res2 = getDistanceBetweenSystems(system, to)

    sumOptions(res1, res2)
  }

  def getDistanceBetweenSystems(from: System, to: String, acc: Int = 0): Option[Int] = {
    from.satellites match {
      case Nil => None
      case x if x.exists(_.body == to) => Some(acc)
      case _ => from.satellites.map(getDistanceBetweenSystems(_, to, acc + 1)).max
    }
  }

  def getOrbitsCount(system: System, depth: Int = 0): Int = {
    if (system.satellites.isEmpty) depth
    else system.satellites.map(getOrbitsCount(_, depth + 1)).sum + depth
  }

  def buildSystem(center: System)(implicit data: List[System]): System = {
    val satellites = findSatellites(center)

    if (satellites.isEmpty) center
    else center hasSatellites satellites.map(buildSystem)
  }

  def findSatellites(system: System)(implicit data: List[System]): List[System] =
    data.filter(_.body == system.body).flatMap(_.satellites)

  def parseInput(input: Iterator[String]): Iterator[System] = for {
    system <- input
    bodies = system.split("\\)")
  } yield System(bodies.head, bodies.tail.toList.map(System(_, List())))

  def sumOptions(a: Option[Int], b: Option[Int]) = {
    for {
      x <- a
      y <- b
    } yield x + y
  }
}
