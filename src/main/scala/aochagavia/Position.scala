package aochagavia

object Position {
  def fromIndex(i: Int, width: Int): Position = Position(i % width, i / width)
  def toIndex(pos: Position, width: Int): Int = pos.x + pos.y * width
}

case class Position(x: Int, y: Int) {
  def +(that: Position): Position = Position(x + that.x, y + that.y)
}
