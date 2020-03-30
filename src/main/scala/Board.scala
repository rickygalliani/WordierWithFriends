import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map 

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

  def getRow(y: Int): Array[Position] = state(Board.Radius - y)

  def getCol(x: Int): Array[Position] = {
    var col = new ListBuffer[Position]
    val colIndex = Board.Radius + x
    (1 * Board.Radius to -1 * Board.Radius by -1).foreach { y =>
      val rowIndex = Board.Radius - y
      col += state(rowIndex)(colIndex)
    }
    col.toArray
  }

  def getRemainingRow(x: Int, y: Int): Array[Position] = {
    getRow(y).slice(Board.Radius + x, Board.Width)
  }

  def getRemainingCol(x: Int, y: Int): Array[Position] = {
    getCol(x).slice(Board.Radius - y, Board.Width)
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

  def moveInValidLocation(move: Move): Boolean = {
    val coordinates = move.getCoordinates()
    val allWithinBoard = coordinates.forall { case (x: Int, y: Int) => Board.validLocation(x, y) }
    val touchesNecessary = if (isEmpty()) {
      // No move has been played yet, move has to touch (0, 0)
      coordinates.contains(0, 0)
    } else {
      // Moves have been played, move needs to touch an existing word
      coordinates.exists { case (x, y) => !openPosition(x, y) }
    }
    allWithinBoard && touchesNecessary
  }

  def moveDoesntOverwrite(move: Move): Boolean = {
    val coordinates = move.getCoordinates()
    val moveLetters = move.word.toList
    var addedALetter = false
    coordinates.zip(moveLetters).foreach { case ((x: Int, y: Int), moveLetter: Char) =>
      if (!openPosition(x, y)) {
        val boardLetter = getLetterAtPosition(x, y)
        // Move cant change any letter already on the board
        if (boardLetter.isDefined && boardLetter.get != moveLetter.toString) return false
      }
      else addedALetter = true
    }
    addedALetter  // A move is "overwriting" if it's copying a word already on the board
  }

  def moveOffshootWords(move: Move): Set[String] = {
    var coordinates = move.getCoordinates()

    // "Offshoot" words include the word includes the tiles at the bookends of the current move
    val beforeCoordinates = {
      if (move.direction == Move.Horizontal) (coordinates.head._1 - 1, coordinates.head._2)
      else (coordinates.head._1, coordinates.head._2 + 1)
    }
    val afterCoordinates = {
      if (move.direction == Move.Horizontal) (coordinates.last._1 + 1, coordinates.last._2)
      else (coordinates.last._1, coordinates.last._2 - 1)  
    }
    val beforeLetter = {
      getLetterAtPosition(beforeCoordinates._1, beforeCoordinates._2).getOrElse("")
    }
    val afterLetter = {
      getLetterAtPosition(afterCoordinates._1, afterCoordinates._2).getOrElse("")
    }
    val bookendWord = {
      if (beforeLetter.nonEmpty || afterLetter.nonEmpty) {
        s"${beforeLetter}${move.word}${afterLetter}"
      }
      else ""
    }
    val letters = move.word.toList.map(_.toString)

    // "Offshoot" words also include words that start from the letters in this move
    coordinates.zip(letters).flatMap { case ((x: Int, y: Int), letter: String) =>
      val (verWord, horWord) = {
        (getVerticalWordAtPosition(x, y, letter), getHorizontalWordAtPosition(x, y, letter))
      }
      var offshootWords = new ListBuffer[String]()
      // Don't include "offshoot" words that are subsets of the bookend word. This happens at the
      // bookends of the move word. Also don't include "offshoot" words that are just the letter.
      if (move.direction == Move.Vertical) {
        if (!bookendWord.contains(verWord) && !verWord.equals(letter)) offshootWords += verWord
        if (!horWord.equals(letter)) offshootWords += horWord
      } else {
        if (!bookendWord.contains(horWord) && !horWord.equals(letter)) offshootWords += horWord
        if (!verWord.equals(letter)) offshootWords += verWord
      }
      offshootWords
    }.toSet ++ (if (bookendWord.nonEmpty) Set(bookendWord) else Set())
  }

  def moveIsValid(move: Move): Boolean = {

    // Validate move has valid coordinates
    val inValidLocation = moveInValidLocation(move: Move)

    // Validate move wouldn't "overwrite" existing tiles
    val notOverwriting = moveDoesntOverwrite(move: Move)

    // Check the validity of the "offshoot" words created
    val offshootWords = moveOffshootWords(move: Move)
    val allValidOffshootWords = offshootWords.forall(Dictionary.wordIsValid _)

    inValidLocation && notOverwriting && allValidOffshootWords
  }

  def getFixedLetters(): (Map[Int, List[(Int, String)]], Map[Int, List[(Int, String)]]) = {

    var fixedRowLetters = Map[Int, List[(Int, String)]]()  // row index -> fixed row letters
    var fixedColLetters = Map[Int, List[(Int, String)]]()  // col index -> fixed col letters

    (1 * Board.Radius to -1 * Board.Radius by -1).foreach { y =>
      val row = getRow(y)
      val fixedHorLetters = row.zipWithIndex.flatMap { case (pos, index) =>
        if (!pos.isOpen()) Option((index, pos.getTile.get.letter)) else None
      }.toList
      fixedRowLetters += (y -> fixedHorLetters)
    }

    (1 * Board.Radius to -1 * Board.Radius by -1).foreach { x =>
      val col = getCol(x)
      val fixedVerLetters = col.zipWithIndex.flatMap { case (pos, index) =>
        if (!pos.isOpen()) Option((index, pos.getTile.get.letter)) else None
      }.toList
      fixedColLetters += (x -> fixedVerLetters)
    }

    (fixedRowLetters, fixedColLetters)
  }

  def getMoves(tiles: Set[Tile]): Set[Move] = {
    var moves = new ListBuffer[Move]
    
    // Get map of which letters are already fixed on the board
    val (rowLetters, colLetters) = getFixedLetters()

    // Traverse board in left-to-right and top-down order
    (1 * Board.Radius to -1 * Board.Radius by -1).foreach { y =>
      
      val row = getRow(y)
      val fixedRowLetters = rowLetters(y)
      val fixedRowIndexes = fixedRowLetters.map(_._1)

      (-1 * Board.Radius to 1 * Board.Radius).foreach { x =>
        printf(s"\rIdentifying words starting from ($x, $y)...")
        val col = getCol(x)
        val remainingRow = getRemainingRow(x, y)
        val remainingCol = getRemainingCol(x, y)
        val fixedColLetters = colLetters(x)
        val fixedColIndexes = fixedColLetters.map(_._1)

        // All words are "discovered" from their left-most or top-most position.
        // So, can't start or end a brand new word right before an existing letter
        var horWords = {
          if (fixedRowIndexes.contains(x - 1)) Set[String]()
          else {
            val remainingRowLetters = Board.remainingFixedRowLetters(x, fixedRowLetters)
            getWords(tiles, remainingRowLetters, remainingRow.length)
          }
        }.filter(w => !fixedRowIndexes.contains(x + w.length))
        var verWords = {
          if (fixedColLetters.map(_._1).contains(y - 1)) Set[String]()
          else {
            val remainingColLetters = Board.remainingFixedColLetters(y, fixedColLetters)
            getWords(tiles, remainingColLetters, remainingCol.length)
          }    
        }.filter(w => !fixedColIndexes.contains(y + w.length))
        
        horWords.foreach(w => moves += Move(w, x, y, Move.Horizontal))
        verWords.foreach(w => moves += Move(w, x, y, Move.Vertical))
      }
    }
    
    val numMoves = moves.size
    println(s"\nIdentified $numMoves possible moves.")
    var finalMoves = new ListBuffer[Move]
    moves.zipWithIndex.foreach { case (move, index) =>
      printf(s"\rValidating move ${index + 1}/$numMoves...")
      if (moveIsValid(move)) finalMoves += move
    }
    println(s"\nReturning ${finalMoves.size} valid moves.")
    finalMoves.toSet
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
    math.abs(x) <= Radius && math.abs(y) <= Radius
  }

  def remainingFixedRowLetters(x: Int, fixedLetters: List[(Int, String)]): List[(Int, String)] = {
    fixedLetters
      .filter(_._1 >= x)  // Filter out ones that are before this starting position
      .map { case (index, word) =>  // Set x to be 0 "start of word"
        ((index + Radius) - (x + Radius), word)
      }
      .sortBy(_._1)
  }

  def remainingFixedColLetters(y: Int, fixedLetters: List[(Int, String)]): List[(Int, String)] = {
    fixedLetters
      .filter(_._1 <= y)  // Filter out ones that are before this starting position
      .map { case (index, word) =>  // Set y to be 0 "start of word"
        ((y + Radius) - (index + Radius), word)
      }
      .sortBy(_._1)
  }

}
