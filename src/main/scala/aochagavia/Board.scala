package aochagavia

trait Board {
  val mines: Int
  val width: Int
  def height: Int
  def cells: Iterable[Cell]
  def getCell(pos: Position): Cell
  def reveal(pos: Position): Board
  def toggleMark(pos: Position): Board
}

object Board {
  def outcome(board: Board): Outcome = {
    // Lose condition: a mine is shown
    val defeat = board.cells.filter(!_.hidden).exists(_.kind match {
      case Mine => true
      case _ => false
    })

    // Win condition: everything is shown besides the mines
    val victory = board.cells.count(_.hidden) == board.mines

    if (defeat) Defeat
    else if (victory) Victory
    else Ongoing
  }

  def display(board: Board): String = {
    val buf = new StringBuilder

    // First line displays column numbers
    buf.append("     ")
    for (x <- 0 until board.width) {
      val padding = if (x < 9) { "  " } else { " " }
      buf.append(padding)
      buf.append(x + 1)
    }

    // Horizontal line to separate column numbers from the rest of the board
    buf.append("\n     ")
    buf.append("_" * 3 * board.width)
    buf.append("\n")

    // Following lines begin with their row number
    for (y <- 0 until board.height) {
      val padding = if (y < 9) { "  " } else { " " }
      buf.append(padding)
      buf.append(y + 1)
      buf.append(" |")

      // Cells
      for (x <- 0 until board.width) {
        val cell = board.getCell(Position(x, y))
        buf.append(Cell.display(cell))
      }

      buf.append("\n\n")
    }

    buf.toString
  }
}
