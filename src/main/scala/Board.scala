import Position.{rp, dl, tl, dw, tw}

class Board {

  private val state: Array[Array[Position]] = Array(
    Array(rp, rp, rp, tw, rp, rp, tl, rp, tl, rp, rp, tw, rp, rp, rp),
    Array(rp, rp, dl, rp, rp, dw, rp, rp, rp, dw, rp, rp, dl, rp, rp),
    Array(rp, dl, rp, rp, dl, rp, rp, rp, rp, rp, dl, rp, rp, dl, rp),
    Array(tw, rp, rp, tl, rp, rp, rp, dw, rp, rp, rp, tl, rp, rp, tw),
    Array(rp, rp, dl, rp, rp, rp, dl, rp, dl, rp, rp, rp, dl, rp, rp),
    Array(rp, dw, rp, rp, rp, tl, rp, rp, rp, tl, rp, rp, rp, dw, rp),
    Array(tl, rp, rp, rp, dl, rp, rp, rp, rp, rp, dl, rp, rp, rp, tl),
    Array(rp, rp, rp, dw, rp, rp, rp, rp, rp, rp, rp, dw, rp, rp, rp),
    Array(tl, rp, rp, rp, dl, rp, rp, rp, rp, rp, dl, rp, rp, rp, tl),
    Array(rp, dw, rp, rp, rp, tl, rp, rp, rp, tl, rp, rp, rp, dw, rp),
    Array(rp, rp, dl, rp, rp, rp, dl, rp, dl, rp, rp, rp, dl, rp, rp),
    Array(tw, rp, rp, tl, rp, rp, rp, dw, rp, rp, rp, tl, rp, rp, tw),
    Array(rp, dl, rp, rp, dl, rp, rp, rp, rp, rp, dl, rp, rp, dl, rp),
    Array(rp, rp, dl, rp, rp, dw, rp, rp, rp, dw, rp, rp, dl, rp, rp),
    Array(rp, rp, rp, tw, rp, rp, tl, rp, tl, rp, rp, tw, rp, rp, rp)
  )

  private def getGridCoordinates(x: Int, y: Int): (Int, Int) = {
    if (math.abs(x) > 7 || math.abs(y) > 7) {
      throw new IllegalArgumentException(s"x and y must be < 8, received (x, y) = ($x, $y)")
    }
    (7 - y, 7 + x)
  }

  private def openPosition(x: Int, y: Int): Boolean = {
    println(s"Called openPosition with $x, $y")
    if (math.abs(x) > 7 || math.abs(y) > 7 || this.getPosition(x, y).getTile.isDefined) false 
    else true
  }

  private def getPosition(x: Int, y: Int): Position = {
    println(s"Called getPosition with $x, $y")
    val (gridX, gridY) = getGridCoordinates(x, y)
    this.state(gridX)(gridY)
  }

  private def setPosition(x: Int, y: Int, tile: Tile): Unit = {
    println(s"Called setPosition with $x, $y, ${tile.letter}")
    val (gridX, gridY) = getGridCoordinates(x, y)
    this.state(gridX)(gridY).setTile(tile)
  }

  def addWord(word: String, startX: Int, startY: Int, dir: Int = Board.Horizontal): Boolean = {
    val coordinates = {
      if (dir == Board.Horizontal) (0 until word.length).map(addX => (startX + addX, startY))
      else (0 to word.length).map(addY => (startX, startY + addY))
    }
    println(s"coordinates here = ${coordinates.mkString(", ")}")
    // Check no tile has been defined before
    coordinates.foreach { case (x, y) => if (!this.openPosition(x, y)) return false }
    println(s"coordinates = ${coordinates.mkString(", ")}")
    // Construct tiles for each character in the word
    val tiles = word.map(c => Tiles.makeTile(c.toString))
    // Add the tiles to the board
    coordinates.zip(tiles).foreach { case ((x, y), tile) => 
      this.setPosition(x, y, tile) 
      this.print()
      println("\n\n")
    }
    true
  }

  // def getMoves(tiles: Set[Tile]): Set[Move] = {

  // }

  def print(): Unit = {
    val stateString = state.map { row =>
      s"| ${row.map { pos => pos.getTile.getOrElse("-") }.mkString(" | ")} |"
    }.mkString("\n")
    println(stateString)
  }

}

object Board {

  val Horizontal = 0
  val Vertical = 1

}