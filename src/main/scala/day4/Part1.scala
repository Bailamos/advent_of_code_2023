package day4

import scala.io.Source

object Part1 extends App {
  val games = Source.fromResource("day4/input.txt").mkString.split("\n")

  val points = games
    .map(parseLineToGame)
    .map(findWinningSelectedNumbers)
    .map(calculatePoints)
    .sum

  println(points)

  def parseLineToGame(line: String): Game = {
    line match {
      case s"Card $index: $winningNumbers | $selectedNumbers" =>
        val winningNumbersArray = winningNumbers.split(' ').filterNot(_.isBlank).map(_.trim.toInt)
        val selectedNumbersArray = selectedNumbers.split(' ').filterNot(_.isBlank).map(_.trim.toInt)

        Game(index.trim.toInt, winningNumbersArray, selectedNumbersArray)
      case _ => throw new Error("Unknown line pattern")
    }
  }

  def findWinningSelectedNumbers(game: Game): Array[Int] = {
    game.winningNumbers.intersect(game.selectedNumbers)
  }

  def calculatePoints(winningSelectedNumbers: Array[Int]): Int = {
    Math.pow(2, winningSelectedNumbers.length - 1).toInt
  }
}