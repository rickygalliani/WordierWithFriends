import scala.io.StdIn.readLine

object Main extends App {

  println("WordierWithFriends")

  val board = new Board()

  // val tiles = readLine("What tiles do you have? (separate with commas (i.e., A, B, C))\n")
  //   .split(", ")
  //   .map(c => Tiles.makeTile(c))
  //   .toSet

  val tiles = List("U", "I", "L", "D", "U", "W", "E").map(c => Tiles.makeTile(c)).toSet

  board.print

  val moves = board.getMoves(tiles).map(m => (m, board.getMoveScore(m)))
  board.makeMove(moves.maxBy(_._2)._1)

  board.print

}
