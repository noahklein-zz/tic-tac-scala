package tictactoe

trait Player
case object X extends Player
case object O extends Player

import Board._

case class Move(player: Player, pos: Int)

case class Board(val positions: List[Pos] = emptyBoard) {
  
  def full = !positions.flatten.contains(None)

  private val didPlayerWin = winFor _ compose playerPositions _

  def won: Option[Player] = (didPlayerWin(X), didPlayerWin(O)) match {
    case (true, _) => Some(X)
    case (_, true) => Some(O)
    case _  => None
  }

  def winFor(playerPositions: PlayerPositions): Boolean =
    !winningPositions.filter(_ subsetOf playerPositions).isEmpty

  def makeMove(move: Move): Board = move.player match {
    case X => new Board(positions.updated(move.pos, Some(X)))
    case O => new Board(positions.updated(move.pos, Some(O)))
  }

  def playerPositions(player: Player): PlayerPositions = player match {
    case X => positions.zipWithIndex.collect{ case (Some(X), i) => i}.toSet
    case O => positions.zipWithIndex.collect{ case (Some(O), i) => i}.toSet
  }

  def emptyPositions: PlayerPositions = positions.zipWithIndex.collect{ case (None, i) => i}.toSet

  override def toString: String = {
    val boardMatrix = positions.zipWithIndex.grouped(3).toList
    boardMatrix.map(_.map(squareToString) mkString " ") mkString "\n"
  }
}


object Board {
  type Pos = Option[Player]
  type PlayerPositions = Set[Int]

  val emptyBoard: List[Pos] = List.fill(9)(None)
  val winningPositions = Set(
      Set(0, 1, 2),
      Set(3, 4, 5),
      Set(6, 7, 8),
      Set(0, 3, 6),
      Set(1, 4, 7),
      Set(2, 5, 8),
      Set(0, 4, 8),
      Set(2, 4, 6))

  def squareToString: Tuple2[Pos, Int] => String = _ match {
    case (Some(X), _) => "X"
    case (Some(O), _) => "O"
    case (_, i) => (i + 1) toString
  }
}
