import org.scalatest.FunSuite

import Position._

class PositionSpec extends FunSuite {

  test("equals(): case 1") { assert(rp == rp) }

  test("equals(): case 2") { assert(dl == dl) }

  test("equals(): case 3") { assert(tl == tl) }

  test("equals(): case 4") { assert(dw == dw) }

  test("equals(): case 5") { assert(tw == tw) }

  test("isOpen(): case 1") { assert(rp.isOpen()) }

  test("isOpen(): case 2") {
    val a = rp
    a.setTile(A)
    assert(!a.isOpen())
  }

}