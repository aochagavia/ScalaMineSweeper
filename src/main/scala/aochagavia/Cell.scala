package aochagavia

object Cell {
  def display(cell: Cell): String = {
    if (cell.hidden) {
      if (cell.marked) "  #"
      else "  *"
    } else cell.kind match {
      case Empty(0) => "   "
      case Empty(x) => s"  $x"
      case Mine => "  X"
    }
  }
}

sealed class Cell(var hidden: Boolean, var marked: Boolean, var kind: CellKind)
sealed trait CellKind
case class Empty(surroundingMines: Int) extends CellKind
case object Mine extends CellKind
