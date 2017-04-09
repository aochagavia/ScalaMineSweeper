package aochagavia

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object ImmutableBoard {
  def create(width: Int, height: Int): ImmutableBoard = {
    val cells = for {
      x <- 0 until width
      y <- 0 until height
    } yield (Position(x, y), new Cell(hidden = true, marked = false, kind = Empty(0)))

    new ImmutableBoard(Map(cells: _*), width, mines = 10, minesSpawned = false)
  }

  private def updateCell(cellMap: Map[Position, Cell], pos: Position, update: Cell => Cell): Map[Position, Cell] =
    cellMap + (pos -> update(cellMap(pos)))
}

case class ImmutableBoard(cellMap: Map[Position, Cell], width: Int, mines: Int, minesSpawned: Boolean) extends Board {
  require(mines < width * height)

  def cells: Iterable[Cell] = cellMap.values
  def height: Int = cellMap.size / width
  override def getCell(pos: Position): Cell = cellMap(pos)

  // -- User actions

  def reveal(pos: Position): ImmutableBoard = {
    // Do nothing if the position is invalid
    cellMap.get(pos) match {
      case None => return this
      case Some(_) =>
    }

    // Spawn mines the first time a reveal happens, so the player never loses the first turn
    val board =
      if (minesSpawned) this
      else spawnMines(pos)

    // Show this and surrounding cells
    board.showCells(Queue(pos))
  }

  def toggleMark(pos: Position): ImmutableBoard =
    this.copy(cellMap = ImmutableBoard.updateCell(cellMap, pos, c => c.copy(marked = !c.marked)))

  // -- Utility methods --

  @tailrec private def showCells(positions: Queue[Position]): ImmutableBoard = {
    positions.dequeueOption match {
      case None => this
      case Some((pos, queue)) =>
        val c = getCell(pos)
        val newQueue = c.kind match {
          case Empty(0) =>
            val surrounding = Position.surroundings(pos, width, height).filter(p => {
              val c = getCell(p)
              c.hidden && c.kind != Mine
            })
            queue.enqueue(surrounding)
          case _ => queue
        }

        val newBoard = this.copy(cellMap = ImmutableBoard.updateCell(cellMap, pos, _.copy(hidden = false)))
        newBoard.showCells(newQueue)
    }
  }

  private def spawnMines(protectedPos: Position): ImmutableBoard = {
    // Spawn the mines
    val minePositions = Position.randoms(width, height).distinct.filter(_ != protectedPos).take(mines)
    val minedCell = Cell(hidden = true, marked = false, kind = Mine)
    val minedMap = minePositions.foldLeft(cellMap)((map, pos) => map + (pos -> minedCell))

    // Store surrounding mine counts in empty cells
    val initMap = minePositions.flatMap(Position.surroundings(_, width, height)).foldLeft(minedMap)((map, pos) => {
      ImmutableBoard.updateCell(map, pos, c => c.kind match {
        case Empty(x) => c.copy(kind = Empty(x + 1))
        case _ => c
      })
    })

    this.copy(cellMap = initMap, minesSpawned = true)
  }
}
