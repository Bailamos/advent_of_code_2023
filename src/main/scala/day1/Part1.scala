package day1

import scala.io.Source

object Part1 extends App {
  val input = Source.fromResource("day1/input_day1_part1.txt").mkString.split("\n")

  val result = input
    .map(removeLettersFromString)
    .map(getFirstAndLastLetter)
    .map(_.toInt)
    .sum

  println(result)

  def removeLettersFromString(text: String): String = {
    text.filter(letter => !letter.isLetter)
  }

  def getFirstAndLastLetter(text: String): String = {
    text.head.toString + text.last.toString
  }
}