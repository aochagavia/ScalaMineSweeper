package aochagavia

import scala.collection.mutable
import scala.util.Random

object Board {
  def create(width: Int, height: Int): Board = {
    val cells = Array.fill(width * height)(new Cell(hidden = true, marked = false, kind = Empty(0)))
    new Board(cells, width, mines = 10)
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
        val cell = board.getCellInPosition(new Position(x, y)).get
        buf.append(Cell.display(cell))
      }

      buf.append("\n\n")
    }

    buf.toString
  }
}

class Board(cells: Array[Cell], private val width: Int, mines: Int) {
  private var minesSpawned: Boolean = false
  private def height: Int = cells.length / width

  def outcome(): Outcome = {
    // Lose condition: a mine is shown
    val defeat = cells.filter(!_.hidden).exists(_.kind match {
      case Mine => true
      case _ => false
    })

    // Win condition: everything is shown besides the mines
    val victory = cells.count(!_.hidden) == cells.length - mines

    if (defeat) Defeat
    else if (victory) Victory
    else Ongoing
  }

  // -- User actions

  def reveal(pos: Position): Unit = {
    val cell = getCellInPosition(pos) match {
      case Some(c) => c
      case None => return
    }

    // Spawn mines the first time a reveal happens, so the player never loses the first turn
    if (!minesSpawned) {
      spawnMines(cell)
    }

    // Show this and surrounding cells
    val showQueue = new mutable.Queue[Position]()
    showQueue.enqueue(pos)

    while (showQueue.nonEmpty) {
      val surroundingPos = showQueue.dequeue()

      // Note: we know the cell exists!
      val c = getCellInPosition(surroundingPos).get
      c.hidden = false

      // Show surrounding cells in case this cell has no surrounding mines
      c.kind match {
        case Empty(0) =>
          val surrounding = surroundingPositions(surroundingPos).filter(p => {
            val c = getCellInPosition(p).get
            c.hidden && c.kind != Mine
          })
          showQueue.enqueue(surrounding: _*)
        case _ =>
      }
    }
  }

  def toggleMark(pos: Position): Unit = getCellInPosition(pos).foreach(c => c.marked = !c.marked)

  // -- Utility methods --

  private def surroundingPositions(pos: Position): Seq[Position] = {
    val shiftUpLeft = new Position(-1, -1)
    (0 until 9).map(i => Position.fromIndex(i, 3) + shiftUpLeft + pos)
               .filterNot(p => p.x == pos.x && p.y == pos.y) // Skip current position
               .filter(validPosition)
  }
  private def surroundingCells(pos: Position): Seq[Cell] = surroundingPositions(pos).flatMap(getCellInPosition)

  private def getCellInPosition(pos: Position): Option[Cell] = {
    if (validPosition(pos))
      Some(cells(Position.toIndex(pos, width)))
    else
      None
  }
  def validPosition(pos: Position): Boolean = 0 <= pos.x && pos.x < width && 0 <= pos.y && pos.y < height

  private def spawnMines(protectedCell: Cell) = {
    assert(mines < cells.length)
    minesSpawned = true
    val rng = new Random()

    var minesToSpawn = mines
    while (minesToSpawn > 0) {
      val i = rng.nextInt(cells.length)
      val randomCell = cells(i)
      if (randomCell != protectedCell && randomCell.kind != Mine) {
        minesToSpawn -= 1
        randomCell.kind = Mine

        // Update the value of surrounding cells
        for (c <- surroundingCells(Position.fromIndex(i, width))) {
          c.kind match {
            case Empty(x) => c.kind = Empty(x + 1)
            case _ =>
          }
        }
      }
    }
  }
}
