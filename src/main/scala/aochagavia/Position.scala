package aochagavia

object Position {
  def fromIndex(i: Int, width: Int): Position = Position(i % width, i / width)
  def toIndex(pos: Position, width: Int): Int = pos.x + pos.y * width
  def isValid(pos: Position, width: Int, height: Int): Boolean =
    0 <= pos.x && pos.x < width && 0 <= pos.y && pos.y < height
  def surroundings(pos: Position, width: Int, height: Int): Seq[Position] = {
    val shiftUpLeft = new Position(-1, -1)
    (0 until 9).map(i => Position.fromIndex(i, 3) + shiftUpLeft + pos)
      .filter(_ != pos) // Skip current position
      .filter(isValid(_, width, height))
  }
}

case class Position(x: Int, y: Int) {
  def +(that: Position): Position = Position(x + that.x, y + that.y)
}
