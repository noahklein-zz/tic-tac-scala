package com.tictactoe

import org.scalatest._
import Board._

class BoardTest extends FlatSpec {
  "testBoard.won" should "be Some(X)" in {
    val board = List(
        List(Some(X), Some(O), Some(O)),
        List(Some(X), None, Some(X)),
        List(Some(X), None, Some(O)))
    val testBoard = new Board(board)
    assert(testBoard.won == Some(X))
  }

  "testBoard.won" should "be Some(O)" in {
    val board = List(
        List(Some(X), Some(O), Some(X)),
        List(Some(X), Some(O), None),
        List(Some(O), Some(O), Some(X)))
    val testBoard = new Board(board)
    assert(testBoard.won == Some(O))
  }

  "playersPositions" should "be Set(1, 4, 5, 7, 8)" in {
    val board = List(
      List(None, Some(O), Some(X)),
      List(Some(X), Some(O), Some(O)),
      List(Some(X), Some(O), Some(O)))
    val testBoard = new Board(board)
    assert(testBoard.playerPositions(O) == Set(1, 4, 5, 7, 8))
    assert(testBoard.playerPositions(X) == Set(2, 3, 6))
  }
  "board.toString" should "be \nO X O\nO   X\nX O  " in {
    val board = List(
      List(Some(O), Some(X), Some(O)),
      List(Some(O), None, Some(X)),
      List(Some(X), Some(O), None))
    val testBoard = new Board(board)
    assert(testBoard.toString == "O X O\nO   X\nX O  ")
  }
}
