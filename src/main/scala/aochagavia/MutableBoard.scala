package aochagavia

import scala.collection.mutable

object MutableBoard {
  def create(width: Int, height: Int): MutableBoard = {
    val cells = Array.fill(width * height)(new Cell(hidden = true, marked = false, kind = Empty(0)))
    new MutableBoard(cells, width, mines = 10)
  }
}

class MutableBoard(cellArr: Array[Cell], val width: Int, val mines: Int) extends Board {
  require(mines < width * height)
  private var minesSpawned: Boolean = false

  def cells: Iterable[Cell] = cellArr
  def height: Int = cellArr.length / width

  def getCell(pos: Position): Cell = {
    assert(Position.isValid(pos, this.width, this.height))
    this.cellArr(Position.toIndex(pos, this.width))
  }

  // -- User actions

  def reveal(pos: Position): MutableBoard = {
    if (!Position.isValid(pos, width, height)) return this

    // Spawn mines the first time a reveal happens, so the player never loses the first turn
    if (!minesSpawned) {
      spawnMines(pos)
    }

    // Show this and surrounding cells
    val showQueue = new mutable.Queue[Position]()
    showQueue.enqueue(pos)

    while (showQueue.nonEmpty) {
      val surroundingPos = showQueue.dequeue()

      // Note: we know the cell exists!
      val c = getCell(surroundingPos)
      updateCell(surroundingPos, c.copy(hidden = false))

      // Show surrounding cells in case this cell has no surrounding mines
      c.kind match {
        case Empty(0) =>
          val surrounding = Position.surroundings(surroundingPos, width, height).filter(p => {
            val c = getCell(p)
            c.hidden && c.kind != Mine
          })
          showQueue.enqueue(surrounding: _*)
        case _ =>
      }
    }

    this
  }

  def toggleMark(pos: Position): MutableBoard = {
    if (Position.isValid(pos, width, height)) updateCell(pos, c => c.copy(marked = !c.marked))
    this
  }

  // -- Utility methods --

  private def updateCell(pos: Position, cell: Cell): Unit = updateCell(pos, _ => cell)

  private def updateCell(pos: Position, updateCell: Cell => Cell): Unit = {
    assert(Position.isValid(pos, width, height))
    val i = Position.toIndex(pos, width)
    cellArr(i) = updateCell(cellArr(i))
  }

  private def spawnMines(protectedPos: Position): Unit = {
    minesSpawned = true
    val minePositions = Position.randoms(width, height).distinct.filter(_ != protectedPos).take(mines)
    val minedCell = Cell(hidden = true, marked = false, kind = Mine)

    for (minePos <- minePositions) {
      // Plant mine
      updateCell(minePos, minedCell)

      // Update the value of surrounding cells
      for (pos <- Position.surroundings(minePos, width, height)) {
        val cell = getCell(pos)
        cell.kind match {
          case Empty(x) => updateCell(pos, cell.copy(kind = Empty(x + 1)))
          case _ =>
        }
      }
    }
  }
}
