package com.tictactoe

trait Player
case object X extends Player
case object O extends Player

import Board._
case class Board(var positions: List[List[Pos]] = emptyBoard) {

  def full() = !positions.flatten.contains(None)

  def won(): Option[Player] = (winFor(playerPositions(X)), winFor(playerPositions(O))) match {
    case (true, _) => Some(X)
    case (_, true) => Some(O)
    case _ => None
  }

  def winFor(playerPositions: PlayerPositions): Boolean = {
    for (win <- winningPositions) {
      if (win.subsetOf(playerPositions)) {
        return true
      }
    }
    return false
  }

  def playerPositions(player: Player): PlayerPositions = player match {
    case X => positions.flatten.zipWithIndex.collect{ case (Some(X), i) => i}.toSet
    case O => positions.flatten.zipWithIndex.collect{ case (Some(O), i) => i}.toSet
  }

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
}
