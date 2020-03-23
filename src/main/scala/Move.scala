case class Move(word: String, startX: Int, startY: Int, direction: Int) {

  def getCoordinates(): List[(Int, Int)] = {
    if (this.direction == Move.Horizontal) {
      (0 until this.word.length).toList.map(addX => (this.startX + addX, this.startY))
    }
    else (0 to this.word.length).toList.map(addY => (this.startX, this.startY - addY))
  }

}

object Move {

  val Horizontal = 0
  val Vertical = 1

}