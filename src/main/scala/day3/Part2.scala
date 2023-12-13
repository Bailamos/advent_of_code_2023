package day3

import scala.io.Source

object Part2 extends App {
  val map = Source.fromResource("day3/input.txt").mkString.split("\n")

  val width = map.length
  val height = map(0).length

  val numbers = map
    .zipWithIndex
    .flatMap{ case (line, row) => findNumbersInLine(line, row) }

  val gearRatio = map
    .zipWithIndex
    .flatMap{ case (line, row) => findGearsInLine(line, row) }
    .map(getNumbersAdjacentToGear)
    .filter(getNumbersAdjacentToGearWithTwoNumbers)
    .map(calculateGearRatio)
    .sum

  println(gearRatio)


  def findNumbersInLine(line: String, rowNumber: Int): Array[Number] = {
    val numberPattern = "[0-9]+".r
    numberPattern.findAllMatchIn(line).map(number => {
      Number(number.toString.toInt, number.start, rowNumber)
    }).toArray
  }

  def isNumbersNearSymbol(number: Number): Boolean = {
    val positions = number.index until number.index + number.value.toString.length

    for {
      position <- positions
      if checkIfDigitIsNearSymbol(position, number.line)
    } return true

    false
  }

  def checkIfDigitIsNearSymbol(x: Int, y: Int): Boolean = {
    val positions = List(
      (x - 1, y), // left
      (x + 1, y), // right
      (x, y - 1), // up
      (x, y + 1), // down
      (x+1, y+1), // down right
      (x+1, y-1), // up right
      (x-1, y-1), // down left
      (x-1, y+1), // up left
    )

    for {
      position <- positions
      if position._1 >= 0 && position._1 < height
      if position._2 >= 0 && position._2 < width
      if map(position._2)(position._1) != '.'
      if !map(position._2)(position._1).isDigit
    } return true

    false
  }

  def findGearsInLine(line: String, rowNumber: Int): Array[Gear] = {
    val numberPattern = "\\*".r
    numberPattern.findAllMatchIn(line).map(number => {
     Gear(number.start, rowNumber)
    }).toArray
  }

  def getNumbersAdjacentToGear(gear: Gear): Array[Number] = {
    for {
      number <- numbers
      if isGearAdjacentToNumber(gear, number)
    } yield number
  }

  def getNumbersAdjacentToGearWithTwoNumbers(number: Array[Number]): Boolean = {
    number.length == 2
  }

  def isGearAdjacentToNumber(gear: Gear, number: Number): Boolean = {
    val numberPosition = number.index until number.index + number.value.toString.length
    val numberCords = numberPosition.map((_, number.line))

    val Gear(x, y) = gear
    val gearVicinity = List(
      (x - 1, y), // left
      (x + 1, y), // right
      (x, y - 1), // up
      (x, y + 1), // down
      (x+1, y+1), // down right
      (x+1, y-1), // up right
      (x-1, y-1), // down left
      (x-1, y+1), // up left
    )

    gearVicinity.exists(
      pos => numberCords.exists(numpos => numpos._1 == pos._1 && numpos._2 == pos._2)
    )
  }

  def calculateGearRatio(number: Array[Number]): Int = {
    number(0).value * number(1).value
  }
}