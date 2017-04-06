package aochagavia

abstract class Command {
  def run(board: Board): Unit
}
class Reveal(pos: Position) extends Command {
  def run(board: Board): Unit = board.reveal(pos)
}
class ToggleMark(pos: Position) extends Command {
  def run(board: Board): Unit = board.toggleMark(pos)
}

object CommandParser {
  def parse(cmd: String): Option[Command] = {
    val parts = cmd.split(" ")
    if (parts.length < 3) {
      return None
    }

    val pos = try {
      val x = parts(1).toInt - 1
      val y = parts(2).toInt - 1
      new Position(x, y)
    } catch {
      case _: Exception => return None
    }

    val command = parts(0) match {
      case "m" => new ToggleMark(pos)
      case "r" => new Reveal(pos)
      case _ => return None
    }

    Some(command)
  }
}
