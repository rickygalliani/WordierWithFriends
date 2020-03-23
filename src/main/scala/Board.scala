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

  private def isEmpty(): Boolean = {
    this.state.foreach { row => row.foreach(pos => if (pos.getTile.isDefined) return false) }
    true
  }

  private def getGridCoordinates(x: Int, y: Int): (Int, Int) = {
    if (math.abs(x) > Board.Radius || math.abs(y) > Board.Radius) {
      throw new IllegalArgumentException(
        s"x and y must be <= ${Board.Radius}, received (x, y) = ($x, $y)"
      )
    }
    (Board.Radius - y, Board.Radius + x)
  }

  private def openPosition(x: Int, y: Int): Boolean = {
    if (math.abs(x) > Board.Radius || 
        math.abs(y) > Board.Radius || 
        this.getPosition(x, y).getTile.isDefined) {
      false 
    }
    else true
  }

  private def getPosition(x: Int, y: Int): Position = {
    val (gridX, gridY) = getGridCoordinates(x, y)
    this.state(gridX)(gridY)
  }

  private def setPosition(x: Int, y: Int, tile: Tile): Unit = {
    val (gridX, gridY) = getGridCoordinates(x, y)
    this.state(gridX)(gridY).setTile(tile)
  }

  def getMoveScore(move: Move): Int = {
    val coordinates = {
      if (move.direction == Move.Horizontal) {
        (0 until move.word.length).map(addX => (move.startX + addX, move.startY))
      }
      else (0 to move.word.length).map(addY => (move.startX, move.startY - addY))
    }
    val tiles = move.word.map(c => Tiles.makeTile(c.toString))
    var score = 0
    var wordFactor = 1
    coordinates.zip(tiles).foreach { case ((x, y), tile) => 
      val pos = this.getPosition(x, y)
      score += tile.points * pos.letterFactor
      wordFactor *= pos.wordFactor
    }
    score *= wordFactor
    score
  }

  def getMoves(tiles: Set[Tile]): Set[Move] = {
    // If the board is empty, compute all the moves with the given tiles
    if (this.isEmpty()) {
      val validWords = Word.getWords(tiles, (0 to tiles.size).toSet)
      validWords.map(w => Move(w, 0, 0, Move.Horizontal))   
    }
    else Set[Move]()

    // Go until you hit an occupied tile
  }

  // TODO: change to makeMove(move: Move)
  def addWord(word: String,
              startX: Int,
              startY: Int,
              direction: Int = Move.Horizontal): Unit = {
    val coordinates = {
      if (direction == Move.Horizontal) (0 until word.length).map(addX => (startX + addX, startY))
      else (0 to word.length).map(addY => (startX, startY - addY))
    }
    // Check no tile has been defined before
    coordinates.foreach { case (x, y) => 
      if (!this.openPosition(x, y)) throw new IllegalArgumentException("Position is already taken")
    }
    // Construct tiles for each character in the word
    val tiles = word.map(c => Tiles.makeTile(c.toString))
    // Add the tiles to the board
    coordinates.zip(tiles).foreach { case ((x, y), tile) => this.setPosition(x, y, tile) }
  }

  def print(): Unit = {
    val rows = this.state.map { row =>
      s"| ${row.map { pos => pos.getTile.getOrElse("-") }.mkString(" | ")} |"
    }.mkString("\n")
    println(s"\n$rows\n")
  }

}

object Board {

  val Width = 15
  val Radius = Width / 2

}
