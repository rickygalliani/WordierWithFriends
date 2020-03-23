import scala.collection.mutable.ListBuffer 

import Position.{rp, dl, tl, dw, tw}
import Word.getWords

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
    state.foreach { row => row.foreach(pos => if (pos.getTile.isDefined) return false) }
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

  private def validLocation(x: Int, y: Int): Boolean = {
    math.abs(x) <= Board.Radius && math.abs(y) <= Board.Radius
  }

  private def openPosition(x: Int, y: Int): Boolean = {
    // Not open if (x, y) isn't a location on board or tile already occupies location
    if (!validLocation(x, y) || getPosition(x, y).getTile.isDefined) false
    else true
  }

  private def getPosition(x: Int, y: Int): Position = {
    val (gridX, gridY) = getGridCoordinates(x, y)
    state(gridX)(gridY)
  }

  // Sets the tile at the position given by the coordinates
  private def setTileAtPosition(x: Int, y: Int, tile: Tile): Unit = {
    val (gridX, gridY) = getGridCoordinates(x, y)
    state(gridX)(gridY).setTile(tile)
  }

  // Clears the tile at the given position
  private def clearTileAtPosition(x: Int, y: Int): Unit = {
    val (gridX, gridY) = getGridCoordinates(x, y)
    state(gridX)(gridY).clearTile()
  }

  private def getLetterAtPosition(x: Int, y: Int): Option[String] = {
    if (validLocation(x, y)) {
      val tile = getPosition(x, y).getTile
      if (tile.isDefined) Option(tile.get.letter) else None
    } 
    else None
  }

  private def getHorizontalWordsAtPosition(x: Int, y: Int, startLetter: String): String = {
  
    var hWord = startLetter

    // Check letters in horizontal word that extend to the left from the current position
    var leftX = x - 1
    var leftLetter = getLetterAtPosition(leftX, y)
    while (leftLetter.isDefined) {
      hWord = leftLetter.get + hWord
      leftX -= 1
      leftLetter = getLetterAtPosition(leftX, y)
    }

    // Check letters in horizontal word that extend to the right from the current position
    var rightX = x + 1
    var rightLetter = getLetterAtPosition(rightX, y)
    while (rightLetter.isDefined) {
      hWord = hWord + rightLetter.get
      rightX += 1
      rightLetter = getLetterAtPosition(rightX, y)
    }

    hWord
  }

  private def getVerticalWordsAtPosition(x: Int, y: Int, startLetter: String): String = {
  
    var vWord = startLetter

    // Check letters in vertical word that extend up from the current position
    var upY = y + 1
    var upLetter = getLetterAtPosition(x, upY)
    while (upLetter.isDefined) {
      vWord = upLetter.get + vWord
      upY += 1
      upLetter = getLetterAtPosition(x, upY)
    }

    // Check letters in vertical word that extend down from the current position
    var downY = y - 1
    var downLetter = getLetterAtPosition(x, downY)
    while (downLetter.isDefined) {
      vWord = vWord + downLetter.get
      downY -= 1
      downLetter = getLetterAtPosition(x, downY)
    }

    vWord
  }

  private def moveIsValid(move: Move): Boolean = {
    // First check whether making the move would "overwrite" existing tiles
    val coordinates = move.getCoordinates()
    coordinates.tail.foreach { case (x, y) => 
      if (getLetterAtPosition(x, y).isDefined) return false
    }

    // Check the validity of the "offshoot" words created
    val offshootWords = coordinates.zip(move.word.toList).map { case ((x, y), letter) =>    
      if (move.direction == Move.Horizontal) getVerticalWordsAtPosition(x, y, letter.toString)
      else getHorizontalWordsAtPosition(x, y, letter.toString)
    }
    offshootWords.forall(Dictionary.wordIsValid _)
  }

  def getMoveScore(move: Move): Int = {
    val coordinates = move.getCoordinates()
    val tiles = move.word.map(c => Tiles.makeTile(c.toString))
    var score = 0
    var wordFactor = 1
    // TODO: need to add scores for "offshoot" words!
    coordinates.zip(tiles).foreach { case ((x, y), tile) => 
      val pos = getPosition(x, y)
      val alreadyOccupied = pos.getTile.isDefined
      score = {
        if (!alreadyOccupied) score + tile.points * pos.letterFactor
        else score + tile.points
      }
      if (!alreadyOccupied) wordFactor *= pos.wordFactor
    }
    score *= wordFactor
    score
  }

  def getMoves(tiles: Set[Tile]): Set[Move] = {
    // Compute all the moves with the given tiles and set origin at center of board
    if (isEmpty) getWords(tiles, (1 to tiles.size).toSet).map(w => Move(w, 0, 0, Move.Vertical))  
    else {
      // Traverse grid constructing words composed of the given tiles and those already on grid 
      var moves = new ListBuffer[Move]
      (-1 * Board.Radius to 1 * Board.Radius).foreach { x =>
        (-1 * Board.Radius to 1 * Board.Radius).foreach { y =>
          if (!openPosition(x, y)) {  // Met an occupied tile
            val letters = getPosition(x, y).getTile.get.letter
            // Use occupied tile(s) as prefix for horizontal words
            var maxAfterX = x + 1
            while (openPosition(maxAfterX, y)) maxAfterX += 1
            val hAfter = getWords(tiles, (2 until maxAfterX - x).toSet, prefix = letters)
            hAfter.foreach(w => moves += Move(w, x, y, Move.Horizontal))

            // Use occupied tile(s) as suffix for horizontal words
            var maxBeforeX = x - 1
            while (openPosition(maxBeforeX, y)) maxBeforeX -= 1
            val hBefore = getWords(tiles, (2 until maxBeforeX - x).toSet, prefix = letters)
            hBefore.foreach(w => moves += Move(w, x, y, Move.Horizontal))

            // Use occupied tile(s) as prefix for vertical words
            var maxAfterY = y + 1
            while (openPosition(x, maxAfterY)) maxAfterY += 1
            val vAfter = getWords(tiles, (2 until maxAfterY - y).toSet, prefix = letters)
            vAfter.foreach(w => moves += Move(w, x, y, Move.Vertical))

            // Use occupied tile(s) as suffix for vertical words
            var maxBeforeY = y - 1
            while (openPosition(x, maxBeforeY)) maxBeforeY -= 1
            val vBefore = getWords(tiles, (2 until maxBeforeY - y).toSet, prefix = letters)
            vBefore.foreach(w => moves += Move(w, x, y, Move.Vertical))          
          }
        }
      }
      moves.filter(m => moveIsValid(m)).toSet
    }
  }

  def makeMove(move: Move): Unit = {
    val coordinates = move.getCoordinates()
    // Construct tiles for each character in the word
    val tiles = move.word.map(c => Tiles.makeTile(c.toString))
    // Add the tiles to the board
    coordinates.zip(tiles).foreach { case ((x, y), tile) => setTileAtPosition(x, y, tile) }
  }

  def print(): Unit = {
    val rows = state.map { row =>
      s"| ${row.map { pos => pos.getTile.getOrElse("-") }.mkString(" | ")} |"
    }.mkString("\n")
    println(s"\n$rows\n")
  }

}

object Board {

  val Width = 15
  val Radius = Width / 2

}
