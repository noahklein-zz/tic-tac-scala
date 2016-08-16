package tictactoe

import java.util.Random

import HumanPlayer._

abstract class User {
  val id: Player
  def getMove(board: Board): Move
}

case class HumanPlayer(id: Player) extends User {
  def getMove(board: Board): Move = {
    println(board.toString)
    var input = ""
    do {
      input = Console readLine "Enter a number from 1-9: "
    } while (!validInputs(input) || !board.emptyPositions(input.toInt - 1))
    Move(id, input.toInt - 1)
  }
}

object HumanPlayer {
  val validInputs = Set("1", "2", "3", "4", "5", "6", "7", "8", "9")
}

case class RobotPlayer(id: Player) extends User {
  private val opponentId = id match {
    case X => O
    case O => X
  }

  def getMove(board: Board): Move = {
    val rand = new Random(System.currentTimeMillis)
    val positions = board.emptyPositions.toList
    val randIndex = rand.nextInt(positions.length)
    Move(id, positions(randIndex))
  }
  // def score(board: Board): Int = { }
}
