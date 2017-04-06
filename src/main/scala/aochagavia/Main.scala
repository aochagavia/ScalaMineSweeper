package aochagavia

object Main {
  def main(args: Array[String]): Unit = {
    println("Welcome to mine sweeper!")
    println()
    println("You may enter the following commands:")
    println("r x y: reveal the cell at coordinates (x, y)")
    println("m x y: toggle a mark on the cell at coordinates (x, y)")
    println()

    val board = Board.create(10, 10)

    while (true) {
      print(board)
      print("Enter a command: ")
      System.out.flush()

      val line = scala.io.StdIn.readLine()
      if (line == null) {
        // No more lines from stdin
        return
      }

      CommandParser.parse(line) match {
        case Some(cmd) => cmd.run(board)
        case None => println("Invalid command, try again.")
      }

      board.outcome() match {
        case Victory => {
          print(board)
          println("Congratulations, you won!")
          return
        }
        case Defeat => {
          print(board)
          println("Too bad, you lost!")
          return
        }
        case Ongoing =>
      }
    }
  }
}
