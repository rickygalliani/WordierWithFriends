import org.scalatest.FunSuite

import scala.collection.mutable.Map
  
import Position.{rp, dl, tl, dw, tw}

class BoardSpec extends FunSuite {

  test("isEmpty(): case 1") {
    val board = new Board()
    assert(board.isEmpty == true)
  }

  test("isEmpty(): case 2") {
    val board = new Board()
    board.setTileAtPosition(0, 0, A)
    assert(board.isEmpty == false)
  }

  test("getRow(): case 1") {
    val board = new Board()
    val testRow = board.getRow(0)
    val truthRow = Array(rp, rp, rp, dw, rp, rp, rp, rp, rp, rp, rp, dw, rp, rp, rp)
    assert(testRow.zip(truthRow).forall { case (te, tr) => te.equals(tr) })
  }

  test("getRow(): case 2") {
    val board = new Board()
    val testRow = board.getRow(-7)
    val truthRow = Array(rp, rp, rp, tw, rp, rp, tl, rp, tl, rp, rp, tw, rp, rp, rp)
    assert(testRow.zip(truthRow).forall { case (te, tr) => te.equals(tr) })
  }

  test("getCol(): case 1") {
    val board = new Board()
    val testCol = board.getCol(7)
    val truthCol = Array(rp, rp, rp, tw, rp, rp, tl, rp, tl, rp, rp, tw, rp, rp, rp)
    assert(testCol.zip(truthCol).forall { case (te, tr) => te.equals(tr) })
  }

  test("getCol(): case 2") {
    val board = new Board()
    val testCol = board.getCol(-3)
    val truthCol = Array(rp, rp, dl, rp, rp, rp, dl, rp, dl, rp, rp, rp, dl, rp, rp)
    assert(testCol.zip(truthCol).forall { case (te, tr) => te.equals(tr) })
  }

  test("getRemainingRow(): case 1") {
    val board = new Board()
    val testRemainingRow = board.getRemainingRow(3, 7)
    val truthRemainingRow = Array(rp, tw, rp, rp, rp)
    assert(testRemainingRow.zip(truthRemainingRow).forall { case (te, tr) => te.equals(tr) })
  }

  test("getRemainingRow(): case 2") {
    val board = new Board()
    val testRemainingRow = board.getRemainingRow(-1, 4)
    val truthRemainingRow = Array(rp, dw, rp, rp, rp, tl, rp, rp, tw)
    assert(testRemainingRow.zip(truthRemainingRow).forall { case (te, tr) => te.equals(tr) })
  }

  test("getRemainingCol(): case 1") {
    val board = new Board()
    val testRemainingCol = board.getRemainingCol(-3, -3)
    val truthRemainingCol = Array(rp, rp, dl, rp, rp)
    assert(testRemainingCol.zip(truthRemainingCol).forall { case (te, tr) => te.equals(tr) })
  }

  test("getRemainingCol(): case 2") {
    val board = new Board()
    val testRemainingCol = board.getRemainingCol(7, 7)
    val truthRemainingCol = Array[Position]()
    assert(testRemainingCol.zip(truthRemainingCol).forall { case (te, tr) => te.equals(tr) })
  }

  test("getGridCoordinates(): case 1") {
    val board = new Board()
    val (xTest, yTest) = board.getGridCoordinates(1, -4)
    assert(xTest == 11)
    assert(yTest == 8)
  }

  test("getGridCoordinates(): case 2") {
    val board = new Board()
    val (xTest, yTest) = board.getGridCoordinates(7, -7)
    assert(xTest == 14)
    assert(yTest == 14)
  }

  test("getGridCoordinates(): case 3") {
    val board = new Board()
    val (xTest, yTest) = board.getGridCoordinates(-6, 5)
    assert(xTest == 2)
    assert(yTest == 1)
  }

  test("validLocation(): case 1") {
    assert(Board.validLocation(7, 7) == true)
  }

  test("validLocation(): case 2") {
    assert(Board.validLocation(-7, -7) == true)
  }

  test("validLocation(): case 3") {
    assert(Board.validLocation(-0, -8) == false)
  }

  test("validLocation(): case 4") {
    assert(Board.validLocation(-10, 15) == false)
  }

  test("getPosition(): case 1") {
    val board = new Board()
    assert(board.getPosition(0, 0) == rp)
  }

  test("getPosition(): case 2") {
    val board = new Board()
    assert(board.getPosition(-7, 4) == tw)
  }

  test("setTileAtPosition(): case 1") {
    val board = new Board()
    board.setTileAtPosition(0, 0, A)
    val testPosition = rp
    testPosition.setTile(A)
    assert(board.getPosition(0, 0) == testPosition)
  }

  test("setTileAtPosition(): case 2") {
    val board = new Board()
    board.setTileAtPosition(-5, 6, Z)
    val testPosition = dl
    testPosition.setTile(Z)
    assert(board.getPosition(-5, 6) == testPosition)
  }

  test("openPosition(): case 1") {
    val board = new Board()
    assert(board.openPosition(0, 0) == true)
  }

  test("openPosition(): case 2") {
    val board = new Board()
    board.setTileAtPosition(0, 0, R)
    assert(board.openPosition(0, 0) == false)
  }

  test("getLetterAtPosition(): case 1") {
    val board = new Board()
    board.setTileAtPosition(0, 0, R)
    assert(board.getLetterAtPosition(0, 0) == Some("R"))
  }

  test("getLetterAtPosition(): case 2") {
    val board = new Board()
    assert(board.getLetterAtPosition(0, 0) == None)
  }

  test("getHorizontalWordAtPosition(): case 1") {
    val board = new Board()
    board.setTileAtPosition(-1, 0, C)
    board.setTileAtPosition(1, 0, T)
    assert(board.getHorizontalWordAtPosition(0, 0, "A") == "CAT")
  }

  test("getHorizontalWordAtPosition(): case 2") {
    val board = new Board()
    assert(board.getHorizontalWordAtPosition(0, 0, "A") == "A")
  }

  test("getHorizontalWordAtPosition(): case 3") {
    //      0 | 1 | 2 | 3 | 4
    //  1 | C | A | V | A | -
    //  0 | A | B | E | A | M
    // -1 | T | A | X | - | -
    // -2 | - | C | - | - | -
    // -3 | - | K | - | - | -
    val board = new Board()

    // BEAM
    board.setTileAtPosition(1, 0, B)
    board.setTileAtPosition(2, 0, E)
    board.setTileAtPosition(3, 0, A)
    board.setTileAtPosition(4, 0, M)

    // ABACK
    board.setTileAtPosition(1, 1, A)
    board.setTileAtPosition(1, -1, A)
    board.setTileAtPosition(1, -2, C)
    board.setTileAtPosition(1, -3, K)

    // VEX
    board.setTileAtPosition(2, 1, V)
    board.setTileAtPosition(2, -1, X)

    // AVA
    board.setTileAtPosition(3, 1, A)
    
    assert(board.getHorizontalWordAtPosition(0, 1, "C") == "CAVA")
    assert(board.getHorizontalWordAtPosition(0, 0, "A") == "ABEAM")
    assert(board.getHorizontalWordAtPosition(0, -1, "T") == "TAX")
  }

  test("getVerticalWordAtPosition(): case 1") {
    val board = new Board()
    board.setTileAtPosition(0, 1, C)
    board.setTileAtPosition(0, -1, T)
    assert(board.getVerticalWordAtPosition(0, 0, "A") == "CAT")
  }

  test("getVerticalWordAtPosition(): case 2") {
    val board = new Board()
    assert(board.getVerticalWordAtPosition(0, 0, "A") == "A")
  }

  test("getVerticalWordAtPosition(): case 3") {
    //      0 | 1 | 2 | 3 | 4
    //  1 | C | A | V | A | -
    //  0 | A | B | E | A | M
    // -1 | T | A | X | - | -
    // -2 | - | C | - | - | -
    // -3 | - | K | - | - | -
    val board = new Board()

    // CAVA
    board.setTileAtPosition(0, 1, C)
    board.setTileAtPosition(1, 1, A)
    board.setTileAtPosition(2, 1, V)
    board.setTileAtPosition(3, 1, A)

    // TAX
    board.setTileAtPosition(0, -1, T)
    board.setTileAtPosition(1, -1, A)
    board.setTileAtPosition(2, -1, X)

    // ABACK
    board.setTileAtPosition(1, 1, A)
    board.setTileAtPosition(1, -2, C)
    board.setTileAtPosition(1, -3, K)

    assert(board.getVerticalWordAtPosition(0, 0, "A") == "CAT")
    assert(board.getVerticalWordAtPosition(1, 0, "B") == "ABACK")
    assert(board.getVerticalWordAtPosition(2, 0, "E") == "VEX")
  }

  test("moveInValidLocation(): case 1") {
    val board = new Board()
    board.setTileAtPosition(0, 0, T)
    val move = Move("TEST", 0, 0, Move.Horizontal)
    assert(board.moveInValidLocation(move))
  }

  test("moveInValidLocation(): case 2") {
    val board = new Board()
    board.setTileAtPosition(0, -3, T)
    val move = Move("TEST", 0, -3, Move.Vertical)
    assert(board.moveInValidLocation(move))
  }

  test("moveInValidLocation(): case 3") {
    val board = new Board()
    board.setTileAtPosition(0, 0, A)
    val move = Move("TEST", 6, -5, Move.Horizontal)
    assert(!board.moveInValidLocation(move))
  }

  test("moveInValidLocation(): case 4") {
    val board = new Board()
    board.setTileAtPosition(0, 0, A)
    val move = Move("TEST", 0, -4, Move.Vertical)
    assert(!board.moveInValidLocation(move))
  }

  test("moveInValidLocation(): case 5") {
    // Move on empty board needs to touch origin
    val board = new Board()
    val move = Move("TEST", 2, 3, Move.Horizontal)
    assert(!board.moveInValidLocation(move))
  }

  test("moveInValidLocation(): case 6") {
    val board = new Board()
    board.setTileAtPosition(0, 0, A)
    val move = Move("TEST", 2, 3, Move.Horizontal)
    assert(!board.moveInValidLocation(move))
  }

  test("moveDoesntOverwrite(): case 1") {
    val board = new Board()
    board.setTileAtPosition(0, 0, T)
    val move = Move("TEST", 0, 0, Move.Horizontal)
    assert(board.moveDoesntOverwrite(move))
  }

  test("moveDoesntOverwrite(): case 2") {
    val board = new Board()
    board.setTileAtPosition(0, 0, A)
    val move = Move("TEST", 0, 0, Move.Horizontal)
    assert(!board.moveDoesntOverwrite(move))
  }

  test("moveDoesntOverwrite(): case 3") {
    val board = new Board()
    board.setTileAtPosition(2, 0, S)
    val move = Move("TEST", 0, 0, Move.Horizontal)
    assert(board.moveDoesntOverwrite(move))
  }

  test("moveDoesntOverwrite(): case 4") {
    val board = new Board()
    board.setTileAtPosition(3, 0, P)
    val move = Move("TEST", 0, 0, Move.Horizontal)
    assert(!board.moveDoesntOverwrite(move))
  }

  test("moveDoesntOverwrite(): case 5") {
    val board = new Board()
    board.setTileAtPosition(0, 0, T)
    board.setTileAtPosition(1, 0, E)
    board.setTileAtPosition(3, 0, T)
    val move = Move("TEST", 0, 0, Move.Horizontal)
    assert(board.moveDoesntOverwrite(move))
  }

  test("moveDoesntOverwrite(): case 6") {
    // "Overwriting" because just copying an existing move
    val board = new Board()
    board.setTileAtPosition(0, 0, T)
    board.setTileAtPosition(1, 0, E)
    board.setTileAtPosition(2, 0, S)
    board.setTileAtPosition(3, 0, T)
    val move = Move("TEST", 0, 0, Move.Horizontal)
    assert(!board.moveDoesntOverwrite(move))
  }

  test("moveDoesntOverwrite(): case 7") {
    // Not "Overwriting" because in a different direction as existing word
    val board = new Board()
    board.setTileAtPosition(0, 0, T)
    board.setTileAtPosition(1, 0, E)
    board.setTileAtPosition(2, 0, S)
    board.setTileAtPosition(3, 0, T)
    val move = Move("TEST", 0, 0, Move.Vertical)
    assert(board.moveDoesntOverwrite(move))
  }

  test("moveOffshootWords(): case 1") {
    //     0 | 1
    // 1 | I | S
    // 0 | S | O
    val board = new Board()
    board.setTileAtPosition(0, 1, I)
    board.setTileAtPosition(1, 1, S)
    val move = Move("SO", 0, 0, Move.Horizontal)
    assert(board.moveOffshootWords(move) == Set("IS", "SO"))
  }

  test("moveOffshootWords(): case 2") {
    val board = new Board()
    val move = Move("SO", 0, 0, Move.Horizontal)
    assert(board.moveOffshootWords(move) == Set())
  }

  test("moveOffshootWords(): case 3") {
    val board = new Board()
    board.setTileAtPosition(0, 0, S)
    board.setTileAtPosition(1, 0, O)
    board.setTileAtPosition(2, 0, U)
    board.setTileAtPosition(3, 0, L)
    val move = Move("SO", 0, 0, Move.Horizontal)
    assert(board.moveOffshootWords(move) == Set("SOUL"))
  }

  test("moveOffshootWords(): case 4") {
    //      0 | 1 | 2 | 3 | 4
    //  1 | C | A | V | A | -
    //  0 | A | B | E | A | M
    // -1 | T | A | X | - | -
    // -2 | - | C | - | - | -
    // -3 | - | K | - | - | -
    val board = new Board()

    // CAVA
    board.setTileAtPosition(0, 1, C)
    board.setTileAtPosition(1, 1, A)
    board.setTileAtPosition(2, 1, V)
    board.setTileAtPosition(3, 1, A)

    // TAX
    board.setTileAtPosition(0, -1, T)
    board.setTileAtPosition(1, -1, A)
    board.setTileAtPosition(2, -1, X)

    // ABACK
    board.setTileAtPosition(1, 1, A)
    board.setTileAtPosition(1, -2, C)
    board.setTileAtPosition(1, -3, K)

    val move = Move("ABEAM", 0, 0, Move.Horizontal)
    val offshootWords = Set("CAT", "ABACK", "VEX", "AA")
    assert(board.moveOffshootWords(move) == offshootWords)
  }

  test("getFixedLetters(): case 1") {
    val board = new Board()
    val (testFixedRowLetters, testFixedColLetters) = board.getFixedLetters()
    var truthFixedRowLetters = Map[Int, List[(Int, String)]]()
    var truthFixedColLetters = Map[Int, List[(Int, String)]]()
    (-1 * Board.Radius to Board.Radius).foreach { n =>
      truthFixedRowLetters += (n -> List[(Int, String)]())
      truthFixedColLetters += (n -> List[(Int, String)]())
    }
    assert(testFixedRowLetters == truthFixedRowLetters)
    assert(testFixedColLetters == truthFixedColLetters)
  }

  test("getFixedLetters(): case 2") {
    val board = new Board()
    board.setTileAtPosition(0, 0, A)
    board.setTileAtPosition(-7, 7, B)
    board.setTileAtPosition(3, -4, C)
    val (testFixedRowLetters, testFixedColLetters) = board.getFixedLetters()
    var truthFixedRowLetters = Map[Int, List[(Int, String)]]()
    var truthFixedColLetters = Map[Int, List[(Int, String)]]()
    (-1 * Board.Radius to Board.Radius).foreach { n =>
      truthFixedRowLetters += (n -> List[(Int, String)]())
      truthFixedColLetters += (n -> List[(Int, String)]())
    }
    truthFixedRowLetters += (0 -> List((7, "A")))
    truthFixedRowLetters += (7 -> List((0, "B")))
    truthFixedRowLetters += (-4 -> List((10, "C")))
    truthFixedColLetters += (0 -> List((7, "A")))
    truthFixedColLetters += (-7 -> List((0, "B")))
    truthFixedColLetters += (3 -> List((11, "C")))

    assert(testFixedRowLetters == truthFixedRowLetters)
    assert(testFixedColLetters == truthFixedColLetters)
  }

}