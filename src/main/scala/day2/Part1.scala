package day2

import scala.io.Source

object Part1 extends App {
  val input = Source.fromResource("day2/input.txt").mkString.split("\n")

  val sum = input
    .map(parseGameLine)
    .map(getGameSets)
    .map(getMaxBallsInSets)
    .zipWithIndex
    .filter(filterImpossibleSets)
    .map(set => set._2 + 1)
    .sum

  println(sum)

  def parseGameLine(text: String) = {
    text match {
      case s"Game $_: $cubeSequences" => cubeSequences
    }
  }

  def getGameSets(game: String) = {
    game.split(';')
  }

  def getMaxBallsInSets(sets: Array[String]) = {
    val maxNumberOfBallsInAllSets = Set(0, 0, 0)

    sets.foldLeft(maxNumberOfBallsInAllSets)((maxBallsInSets, set) => {
      val ballsInCurrentSet = getBallsInSet(set)

      maxBallsInSets.copy(
        red = if (ballsInCurrentSet.red > maxBallsInSets.red ) ballsInCurrentSet.red else maxBallsInSets.red,
        blue = if (ballsInCurrentSet.blue > maxBallsInSets.blue ) ballsInCurrentSet.blue else maxBallsInSets.blue,
        green = if (ballsInCurrentSet.green > maxBallsInSets.green ) ballsInCurrentSet.green else maxBallsInSets.green
      )
    })
  }

  def getBallsInSet(set: String): Set = {
    var red = 0
    var green = 0
    var blue = 0

    for {
      case s"$number $color" <- set.split(',').map(_.trim)
    } {
      if (color.contains("red")) red = number.toInt
      if (color.contains("green")) green = number.toInt
      if (color.contains("blue")) blue = number.toInt
    }

    Set(red= red, green = green, blue= blue)
  }

  def filterImpossibleSets(set: (Set, _)): Boolean = {
    val maxRed = 12
    val maxGreen = 13
    val maxBlue = 14

    if (set._1.blue > maxBlue || set._1.red > maxRed || set._1.green > maxGreen) return false
    true
  }
}