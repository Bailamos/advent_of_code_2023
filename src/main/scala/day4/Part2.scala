package day4

import scala.io.Source

case class GameWithCount(count: Int = 1, game: Game)

object Part2 extends App {
  val input = Source.fromResource("day4/input.txt").mkString.split("\n")

  val games = input
    .map(parseLineToGame)
    .map(parseToGameWithCount)

  val result = redeemAllScratchCards(games)
    .map(_.count)
    .sum

  println(result)

  def parseLineToGame(line: String): Game = {
    line match {
      case s"Card $index: $winningNumbers | $selectedNumbers" =>
        val winningNumbersArray = winningNumbers.split(' ').filterNot(_.isBlank).map(_.trim.toInt)
        val selectedNumbersArray = selectedNumbers.split(' ').filterNot(_.isBlank).map(_.trim.toInt)

        Game(index.trim.toInt, winningNumbersArray, selectedNumbersArray)
      case _ => throw new Error("Unknown line pattern")
    }
  }

  def parseToGameWithCount(game: Game): GameWithCount = {
    GameWithCount(game = game)
  }

  def redeemAllScratchCards(games: Array[GameWithCount], currentIndex: Int = 0): Array[GameWithCount] = {
    if (currentIndex == games.length) return games

    val updatedGames = redeemScratchCard(currentIndex, games)

    redeemAllScratchCards(updatedGames, currentIndex + 1)
  }

  def redeemScratchCard(currentIndex: Int, games: Array[GameWithCount]) = {
    val current = games(currentIndex)
    val howMany = howManyScratchCardsWon(current.game)
    val scratchCardsWon = currentIndex + 1 to currentIndex + howMany

    games.zipWithIndex.map { case ( game, index ) =>
      if (scratchCardsWon contains index) updateScratchCardNumber(game, current.count)
      else game
    }
  }

  def howManyScratchCardsWon(game: Game): Int = {
    game.winningNumbers.intersect(game.selectedNumbers).length
  }

  def updateScratchCardNumber(game: GameWithCount, count: Int): GameWithCount = {
    game.copy(count = game.count + count)
  }
}