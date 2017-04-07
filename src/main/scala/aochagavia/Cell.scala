package aochagavia

class Cell(var hidden: Boolean, var marked: Boolean, var kind: CellKind) {
  override def toString: String = {
    if (hidden) {
      if (marked) { "  #" } else { "  *" }
    } else {
      kind.toString
    }
  }
}

sealed trait CellKind
case class Empty(surroundingMines: Int) extends CellKind {
  override def toString: String = {
    surroundingMines match {
      case 0 => "   "
      case x => "  " + x
    }
  }
}
case object Mine extends CellKind {
  override def toString: String = {
    "  X"
  }
}
