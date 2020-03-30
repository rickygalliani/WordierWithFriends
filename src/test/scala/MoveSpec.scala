import org.scalatest.FunSuite

class MoveSpec extends FunSuite {

  test("getCoordinates(): case 1") {
    val move: Move = Move("VIN", -1, 3, Move.Vertical)
    val testCoordinates: List[(Int, Int)] = move.getCoordinates()
    assert(testCoordinates == List((-1, 3), (-1, 2), (-1, 1)))
  }

  test("getCoordinates(): case 2") {
    val move: Move = Move("VIN", -1, 3, Move.Horizontal)
    val testCoordinates: List[(Int, Int)] = move.getCoordinates()
    assert(testCoordinates == List((-1, 3), (0, 3), (1, 3)))
  }

}