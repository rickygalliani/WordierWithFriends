import scala.io.StdIn.readLine

object Main extends App {

  println("WordierWithFriends")

  val board = new Board()

  // val tiles = readLine("What tiles do you have? (separate with commas (i.e., A, B, C))\n")
  //   .split(", ")
  //   .map(c => Tiles.makeTile(c))
  //   .toSet

  val tiles = List("U", "I", "L", "D", "U", "W", "E").map(c => Tiles.makeTile(c)).toSet

  val moves = board.getMoves(tiles)
  moves.foreach { move =>
    println(s"$move: ${board.getMoveScore(move)}")
  }

  // board.addWord("TEST", 0, 0)

  // board.print

  // board.addWord("OTHER", -1, 0, dir = Move.Vertical) 

  // board.print

}
