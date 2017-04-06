package aochagavia

class Position(val x: Int, val y: Int) {
  def add(other: Position): Position = {
    new Position(other.x + x, other.y + y)
  }
}
