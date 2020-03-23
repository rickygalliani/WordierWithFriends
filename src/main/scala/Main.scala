import scala.io.StdIn.readLine

object Main extends App {

  println("WordierWithFriends")

  val board = new Board()

  // val tiles = readLine("What tiles do you have? (separate with commas (i.e., A, B, C))\n")
  //   .split(", ")
  //   .map(c => Tiles.makeTile(c))
  //   .toSet

  val tiles = List("F", "A", "M", "I", "L", "Y").map(c => Tiles.makeTile(c)).toSet

  board.print
  val moves = board.getMoves(tiles).map(m => (m, board.getMoveScore(m))).toList.sortBy(_._2)
  moves.foreach(m => println(s"${m._1.asString} ${m._2}"))

  board.makeMove(moves.maxBy(_._2)._1)

  board.print

}
