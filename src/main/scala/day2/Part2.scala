package day2

import scala.io.Source

object Part2 extends App {
  val input = Source.fromResource("day2/input.txt").mkString.split("\n")

  val sum = input
    .map(parseGameLine)
    .map(getGameSets)
    .map(getMaxBallsInSets)
    .map(getPower)
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

  def getPower(set: Set): Int = {
    set.blue * set.red * set.green
  }
}