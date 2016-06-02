package tictactoe


import Game._


case class Game(var board: Board = new Board()) {
  val xPlayer = new HumanPlayer(X)
  val oPlayer = new RobotPlayer(O)
  
  def gameLoop = {
    while (!board.full || board.won == None) {
      val xMove = xPlayer.getMove(board)
      board = board makeMove xMove

      val oMove = oPlayer.getMove(board)
      board = board makeMove oMove
    }

    println(gameOverMessage(board.won))
  }
}


object Game {

  def main(args: Array[String]) = {
    println("starting...")
    val game = new Game()
    game.gameLoop
  }

  def gameOverMessage(winner: Option[Player]) = winner match {
      case Some(X) => "X wins!"
      case Some(O) => "O wins!"
      case _ => "It's a draw."
    }
}
