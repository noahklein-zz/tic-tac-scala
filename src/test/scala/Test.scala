package tictactoe

import org.scalatest._
import Board._
import Game._

class BoardTest extends FlatSpec {
  "testBoard.won" should "be Some(X)" in {
    val board = List(Some(X), Some(O), Some(O),
                     Some(X), None, Some(X),
                     Some(X), None, Some(O))
    val testBoard = new Board(board)
    assert(testBoard.won == Some(X))
  }

  "testBoard.won" should "be Some(O)" in {
    val board = List(Some(X), Some(O), Some(X),
                     Some(X), Some(O), None,
                     Some(O), Some(O), Some(X))
    val testBoard = new Board(board)
    assert(testBoard.won == Some(O))
  }

  "playersPositions" should "be Set(1, 4, 5, 7, 8)" in {
    val board = List(None, Some(O), Some(X),
                     Some(X), Some(O), Some(O),
                     Some(X), Some(O), Some(O))
    val testBoard = new Board(board)
    assert(testBoard.playerPositions(O) == Set(1, 4, 5, 7, 8))
    assert(testBoard.playerPositions(X) == Set(2, 3, 6))
  }

  "emptyPositions" should "be Set(0, 4, 7)" in {
    val board = List(None, Some(O), Some(X),
                     Some(X), None, Some(O),
                     Some(X), None, Some(O))
    val testBoard = new Board(board)
    assert(testBoard.emptyPositions == Set(0, 4, 7))
  }

  "board.toString" should "be \nO X O\nO 5 X\nX O 9" in {
    val board = List(Some(O), Some(X), Some(O),
                     Some(O), None, Some(X),
                     Some(X), Some(O), None)
    val testBoard = new Board(board)
    assert(testBoard.toString == "O X O\nO 5 X\nX O 9")
  }

  "updatedBoard" should "be a" in {
    val board = new Board()
    val newBoard = board.makeMove(new Move(X, 3))
    val modifiedBoard = List(None, None, None,
                             Some(X), None, None,
                             None, None, None)
    assert(newBoard.positions == modifiedBoard)
  }
}
