import scala.collection.mutable.ListBuffer 

import Position.{rp, dl, tl, dw, tw}
import Word.getWords

class Board() {

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

  private val words: ListBuffer[String] = new ListBuffer[String]

  def isEmpty(): Boolean = {
    state.foreach { row => row.foreach(pos => if (pos.getTile.isDefined) return false) }
    true
  }

  def getRow(y: Int): Array[Position] = {
    val rowIndex = Board.Radius - y
    state(rowIndex)
  }

  def getCol(x: Int): Array[Position] = {
    var col = new ListBuffer[Position]
    val colIndex = Board.Radius + x
    (1 * Board.Radius to -1 * Board.Radius by -1).foreach { y =>
      val rowIndex = Board.Radius - y
      col += state(rowIndex)(colIndex)
    }
    col.toArray
  }

  def getGridCoordinates(x: Int, y: Int): (Int, Int) = {
    if (math.abs(x) > Board.Radius || math.abs(y) > Board.Radius) {
      throw new IllegalArgumentException(
        s"x and y must be <= ${Board.Radius}, received (x, y) = ($x, $y)"
      )
    }
    (Board.Radius - y, Board.Radius + x)
  }

  def getPosition(x: Int, y: Int): Position = {
    val (gridX, gridY) = getGridCoordinates(x, y)
    state(gridX)(gridY)
  }

  // Sets the tile at the position given by the coordinates
  def setTileAtPosition(x: Int, y: Int, tile: Tile): Unit = {
    val (gridX, gridY) = getGridCoordinates(x, y)
    state(gridX)(gridY).setTile(tile)
  }

  def openPosition(x: Int, y: Int): Boolean = {
    // Not open if (x, y) isn't a location on board or tile already occupies location
    if (Board.validLocation(x, y) && getPosition(x, y).isOpen()) true
    else false
  }

  def getLetterAtPosition(x: Int, y: Int): Option[String] = {
    if (Board.validLocation(x, y)) {
      val pos = getPosition(x, y)
      if (!pos.isOpen()) Option(pos.getTile.get.letter) else None
    } 
    else None
  }

  def getHorizontalWordAtPosition(x: Int, y: Int, startLetter: String): String = {
  
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

  def getVerticalWordAtPosition(x: Int, y: Int, startLetter: String): String = {
  
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

  def moveIsValid(move: Move): Boolean = {
    // First check whether making the move would "overwrite" existing tiles
    val coordinates = move.getCoordinates()
    coordinates.tail.foreach { case (x, y) => if (!openPosition(x, y)) return false }

    // Check the validity of the "offshoot" words created
    val offshootWords = coordinates.zip(move.word.toList).flatMap { case ((x, y), letter) =>    
      val verWord = getVerticalWordAtPosition(x, y, letter.toString)
      val horWord = getHorizontalWordAtPosition(x, y, letter.toString)
      Seq(verWord, horWord).flatMap(w => if (w.equals(letter.toString)) None else Option(w))
    }
    offshootWords.forall(Dictionary.wordIsValid _)
  }

  def getMoveScore(move: Move): Int = {
    val coordinates = move.getCoordinates()
    val tiles = move.word.map(c => Tiles.makeTile(c.toString))
    var score = 0
    var wordFactor = 1

    coordinates.zip(tiles).foreach { case ((x, y), tile) =>
      // First count score contribution from letters in "base" word
      val pos = getPosition(x, y)
      val alreadyOccupied = pos.getTile.isDefined
      val tilePoints = if (!alreadyOccupied) tile.points * pos.letterFactor else tile.points
      score += tilePoints
      if (!alreadyOccupied) wordFactor *= pos.wordFactor

      // Next count score contribution from letters in "offshoot" words connected to "base" word
      val vertWord = getVerticalWordAtPosition(x, y, tile.letter)
      val horWord = getHorizontalWordAtPosition(x, y, tile.letter)

      // "offshoot" words only come in opposite direction of move otherwise they're "base" word
      // "offshoot" words only count for the score if they were just introduced with this move
      val newVerticalOffshoot = (
        !vertWord.equals(tile.letter) &&
        !words.contains(vertWord) &&
        !move.direction.equals(Move.Vertical)
      )
      val newHorizontalOffshoot = (
        !horWord.equals(tile.letter) &&
        !words.contains(horWord) &&
        !move.direction.equals(Move.Horizontal)
      )
      if (newVerticalOffshoot) {
        val offVert = vertWord.patch(vertWord.lastIndexOf(tile.letter), "", 1)
        score += tilePoints + offVert.map(c => Tiles.makeTile(c.toString).points).sum
      }
      if (newHorizontalOffshoot) {
        val offHor = horWord.patch(horWord.lastIndexOf(tile.letter), "", 1)
        score += tilePoints + offHor.map(c => Tiles.makeTile(c.toString).points).sum
      }
    }
    score *= wordFactor
    score
  }

  // def getMoves(tiles: Set[Tile]): Set[Move] = {
  //   // Compute all the moves with the given tiles and set origin at center of board
  //   if (isEmpty) getWords(tiles, (1 to tiles.size).toSet).map(w => Move(w, 0, 0, Move.Vertical))  
  //   else {
  //     // Traverse grid constructing words composed of the given tiles and those already on grid 
  //     var moves = new ListBuffer[Move]
  //     (-1 * Board.Radius to 1 * Board.Radius).foreach { x =>
  //       (-1 * Board.Radius to 1 * Board.Radius).foreach { y =>
  //         if (!openPosition(x, y)) {  // Met an occupied tile
  //           val letters = getPosition(x, y).getTile.get.letter
  //           // Use occupied tile(s) as prefix for horizontal words
  //           var maxAfterX = x + 1
  //           while (openPosition(maxAfterX, y)) maxAfterX += 1
  //           val hAfter = getWords(tiles, (2 until maxAfterX - x).toSet, prefix = letters)
  //           hAfter.foreach(w => moves += Move(w, x, y, Move.Horizontal))

  //           // Use occupied tile(s) as suffix for horizontal words
  //           var maxBeforeX = x - 1
  //           while (openPosition(maxBeforeX, y)) maxBeforeX -= 1
  //           val hBefore = getWords(tiles, (2 until maxBeforeX - x).toSet, suffix = letters)
  //           hBefore.foreach(w => moves += Move(w, x, y, Move.Horizontal))

  //           // Use occupied tile(s) as prefix for vertical words
  //           var maxAfterY = y + 1
  //           while (openPosition(x, maxAfterY)) maxAfterY += 1
  //           val vAfter = getWords(tiles, (2 until maxAfterY - y).toSet, prefix = letters)
  //           vAfter.foreach(w => moves += Move(w, x, y, Move.Vertical))

  //           // Use occupied tile(s) as suffix for vertical words
  //           var maxBeforeY = y - 1
  //           while (openPosition(x, maxBeforeY)) maxBeforeY -= 1
  //           val vBefore = getWords(tiles, (2 until maxBeforeY - y).toSet, suffix = letters)
  //           vBefore.foreach(w => moves += Move(w, x, y, Move.Vertical))          
  //         }
  //       }
  //     }
  //     moves.filter(m => moveIsValid(m)).toSet
  //   }
  // }

  def getMovesV2(tiles: Set[Tile]): Set[Move] = {
    var moves = new ListBuffer[Move]
    (1 * Board.Radius to -1 * Board.Radius by -1).foreach { y =>
      val row = getRow(y)
      (-1 * Board.Radius to 1 * Board.Radius).foreach { x =>
        val col = getCol(x)
        val remainingRow = row.slice(x, Board.Width)
        val remainingCol = col.slice(y, Board.Width)

        // Compute all words that can start from (x, y) and incorporate letters in row already
        val fixedHorLetters = remainingRow.zipWithIndex.flatMap { case (pos, index) =>
          if (!pos.isOpen()) Option((index, pos.getTile.get.letter)) else None
        }.toList
        val horWords = getWords(tiles, fixedHorLetters, remainingRow.length)

        // Compute all words that can start from (x, y) and incorporate letters in column already
        val fixedVerLetters = remainingCol.zipWithIndex.flatMap { case (pos, index) =>
          if (!pos.isOpen()) Option((index, pos.getTile.get.letter)) else None
        }.toList
        val verWords = getWords(tiles, fixedVerLetters, remainingCol.length)

        horWords.foreach(w => moves += Move(w, x, y, Move.Horizontal))
        verWords.foreach(w => moves += Move(w, x, y, Move.Vertical))
      }
    }
    moves.filter(m => moveIsValid(m)).toSet
  }

  def makeMove(move: Move): Unit = {
    val coordinates = move.getCoordinates()
    // Construct tiles for each character in the word
    val tiles = move.word.map(c => Tiles.makeTile(c.toString))
    // Add the tiles to the board
    coordinates.zip(tiles).foreach { case ((x, y), tile) => setTileAtPosition(x, y, tile) }
    words += move.word
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

  def validLocation(x: Int, y: Int): Boolean = {
    math.abs(x) <= Board.Radius && math.abs(y) <= Board.Radius
  }

}
