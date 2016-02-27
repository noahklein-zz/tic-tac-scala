package com.tictactoe

trait Player
case object X extends Player
case object O extends Player

import Board._
case class Board(var positions: List[List[Pos]] = emptyBoard) {

  def full() = !positions.flatten.contains(None)

  def won(): Option[Player] = (didPlayerWin(X), didPlayerWin(O)) match {
    case (true, _) => Some(X)
    case (_, true) => Some(O)
    case _  => None
  }

  private val didPlayerWin = winFor _ compose playerPositions _

  def winFor(playerPositions: PlayerPositions): Boolean = {
    winningPositions.filter(_ subsetOf playerPositions).toSeq match {
      case Nil => false
      case _   => true
    }
  }

  def playerPositions(player: Player): PlayerPositions = {
    val flattenedBoard = positions.flatten.zipWithIndex
    player match {
      case X => flattenedBoard.collect{ case (Some(X), i) => i}.toSet
      case O => flattenedBoard.collect{ case (Some(O), i) => i}.toSet
    }
  }

  override def toString(): String =
    positions.map(_.map(squareToString) mkString " ") mkString "\n"
}


object Board {
  type Pos = Option[Player]
  type PlayerPositions = Set[Int]

  val emptyBoard: List[List[Pos]] = List.fill(3)(List.fill(3)(None))
  val winningPositions = Set(
      Set(0, 1, 2),
      Set(3, 4, 5),
      Set(6, 7, 8),
      Set(0, 3, 6),
      Set(1, 4, 7),
      Set(2, 5, 8),
      Set(0, 4, 8),
      Set(2, 4, 6))

  def squareToString(player: Option[Player]): String = player match {
    case Some(X) => "X"
    case Some(O) => "O"
    case _ => " "
  }
}
