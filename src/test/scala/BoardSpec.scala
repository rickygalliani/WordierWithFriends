import org.scalatest.FunSuite
  
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

}