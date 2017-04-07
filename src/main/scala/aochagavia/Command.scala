package aochagavia

sealed trait Command {
  def run(board: Board): Unit
}

case class Reveal(pos: Position) extends Command {
  def run(board: Board): Unit = board.reveal(pos)
}

case class ToggleMark(pos: Position) extends Command {
  def run(board: Board): Unit = board.toggleMark(pos)
}

object Command {
  def parse(cmd: String): Option[Command] = {
    val parts = cmd.split(" ")
    if (parts.length < 3) {
      return None
    }

    val pos = try {
      val x = parts(1).toInt - 1
      val y = parts(2).toInt - 1
      Position(x, y)
    } catch {
      case _: Exception => return None
    }

    parts(0) match {
      case "m" => Some(ToggleMark(pos))
      case "r" => Some(Reveal(pos))
      case _ => None
    }
  }
}
