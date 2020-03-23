case class Move(word: String, startX: Int, startY: Int, direction: Int) {

  def getCoordinates(): List[(Int, Int)] = {
    if (direction == Move.Horizontal) {
      (0 until word.length).toList.map(addX => (startX + addX, startY))
    }
    else (0 to word.length).toList.map(addY => (startX, startY - addY))
  }

  def asString(): String = {
    val dir = if (direction == Move.Horizontal) "horizontal" else "vertical"
    s"($startX, $startY) ($dir): '$word'"
  }

  def print(): Unit = println(asString)

}

object Move {

  val Horizontal = 0
  val Vertical = 1

}