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

  private def openPosition(x: Int, y: Int): Boolean = {
    if (math.abs(x) > Board.Radius || 
        math.abs(y) > Board.Radius || 
        getPosition(x, y).getTile.isDefined) {
      false 
    }
    else true
  }

  private def getPosition(x: Int, y: Int): Position = {
    val (gridX, gridY) = getGridCoordinates(x, y)
    state(gridX)(gridY)
  }

  // Sets the tile at the position given by the coordinates
  private def setPosition(x: Int, y: Int, tile: Tile): Unit = {
    val (gridX, gridY) = getGridCoordinates(x, y)
    state(gridX)(gridY).setTile(tile)
  }

  def getMoveScore(move: Move): Int = {
    val coordinates = move.getCoordinates()
    val tiles = move.word.map(c => Tiles.makeTile(c.toString))
    var score = 0
    var wordFactor = 1
    coordinates.zip(tiles).foreach { case ((x, y), tile) => 
      val pos = getPosition(x, y)
      score += tile.points * pos.letterFactor
      wordFactor *= pos.wordFactor
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
            val letters = getPosition(x, y).getTile.get.letter  // TODO: support mutliple letters
            // Use occupied tile(s) as prefix for horizontal words
            var maxAfterX = x + 1
            while (openPosition(maxAfterX, y)) maxAfterX += 1
            val horizontalAfter = getWords(tiles, (2 until maxAfterX - x).toSet, prefix = letters)
            horizontalAfter.foreach(w => moves += Move(w, x, y, Move.Horizontal))

            // Use occupied tile(s) as suffix for horizontal words
            var maxBeforeX = x - 1
            while (openPosition(maxBeforeX, y)) maxBeforeX -= 1
            val horizontalBefore = getWords(tiles, (2 until maxBeforeX - x).toSet, prefix = letters)
            horizontalBefore.foreach(w => moves += Move(w, x, y, Move.Horizontal))

            // Use occupied tile(s) as prefix for vertical words
            var maxAfterY = y + 1
            while (openPosition(x, maxAfterY)) maxAfterY += 1
            val verticalAfter = getWords(tiles, (2 until maxAfterY - y).toSet, prefix = letters)
            verticalAfter.foreach(w => moves += Move(w, x, y, Move.Vertical))

            // Use occupied tile(s) as suffix for vertical words
            var maxBeforeY = y - 1
            while (openPosition(x, maxBeforeY)) maxBeforeY -= 1
            val verticalBefore = getWords(tiles, (2 until maxBeforeY - y).toSet, prefix = letters)
            verticalBefore.foreach(w => moves += Move(w, x, y, Move.Vertical))          
          }
        }
      }
      moves.toSet
    }
  }

  def makeMove(move: Move): Unit = {
    val coordinates = move.getCoordinates()
    // Construct tiles for each character in the word
    val tiles = move.word.map(c => Tiles.makeTile(c.toString))
    // Add the tiles to the board
    coordinates.zip(tiles).foreach { case ((x, y), tile) => setPosition(x, y, tile) }
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
