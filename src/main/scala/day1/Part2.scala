package day1

import scala.io.Source

object Part2 extends App {
  val input = Source.fromResource("day1/input_day1_part2.txt").mkString.split("\n")

  val result = input
    .map(transformTextToNumbers)
    .map(getFirstAndLastLetter)
    .map(_.toInt)
    .sum

  println(result)

  def transformTextToNumbers(text: String): String = {
    text match {
      case s"one$tail" => s"1" + transformTextToNumbers(text.tail)
      case s"two$tail" => s"2" + transformTextToNumbers(text.tail)
      case s"three$tail" => s"3" + transformTextToNumbers(text.tail)
      case s"four$tail" => s"4" + transformTextToNumbers(text.tail)
      case s"five$tail" => s"5" + transformTextToNumbers(text.tail)
      case s"six$tail" => s"6" + transformTextToNumbers(text.tail)
      case s"seven$tail" => s"7" + transformTextToNumbers(text.tail)
      case s"eight$tail" => s"8" + transformTextToNumbers(text.tail)
      case s"nine$tail" => s"9" + transformTextToNumbers(text.tail)
      case s"1$tail" => s"1" + transformTextToNumbers(text.tail)
      case s"2$tail" => s"2" + transformTextToNumbers(text.tail)
      case s"3$tail" => s"3" + transformTextToNumbers(text.tail)
      case s"4$tail" => s"4" + transformTextToNumbers(text.tail)
      case s"5$tail" => s"5" + transformTextToNumbers(text.tail)
      case s"6$tail" => s"6" + transformTextToNumbers(text.tail)
      case s"7$tail" => s"7" + transformTextToNumbers(text.tail)
      case s"8$tail" => s"8" + transformTextToNumbers(text.tail)
      case s"9$tail" => s"9" + transformTextToNumbers(text.tail)
      case "" => ""
      case _ => "" + transformTextToNumbers(text.tail)
    }

  }

  def getFirstAndLastLetter(text: String): String = {
    text.head.toString + text.last.toString
  }
}