package AdventOfCode2019

import scala.io.Source

object Puzzle8 extends App {
  val PICTURE_WIDTH = 25
  val PICTURE_HEIGHT = 6
  val LAYER_LENGTH = PICTURE_WIDTH * PICTURE_HEIGHT

  val input = Source.fromResource("input_ex8").mkString

  //val layers = List("0222", "1122", "2212", "0000")
  val layers = input.grouped(LAYER_LENGTH).toList

  val sorted = layers.sortBy { layer => layer.count(_ == '0') }
  val result = sorted.head.map(_.toInt).count(_ == '1') * sorted.head.map(_.toInt).count(_ == '2')=
  println(result)

  val image = layers.tail.foldLeft(layers.head) { (layer1, layer2) =>
    layer1.zip(layer2).map { case (pixel1, pixel2) =>
        if (pixel1 == '2') pixel2
        else pixel1
    }.mkString
  }
  for {
    i <- image.grouped(PICTURE_WIDTH)
  } println(i)
}
