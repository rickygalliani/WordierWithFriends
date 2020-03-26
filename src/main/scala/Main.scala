import scala.io.StdIn.readLine

object Main extends App {

  println("WordierWithFriends")

  val board = new Board()

  board.makeMove(Move("FUND", -2, 0, Move.Horizontal))

  board.print

  // val tiles = readLine("What tiles do you have? (separate with commas (i.e., A, B, C))\n")
  //   .split(", ")
  //   .map(c => Tiles.makeTile(c))
  //   .toSet

  val tiles = Seq("I", "L", "V", "R", "N", "B", "P").map(c => Tiles.makeTile(c)).toSet

  val moves = board.getMovesV2(tiles)
  println(s"moves = ${moves.mkString("\n")}")
    // .map(m => (m, board.getMoveScore(m)))
    // .toList
    // .sortBy(_._2)(Ordering[Int].reverse)

  // moves.foreach(m => println(s"${m._1.asString} ${m._2}"))
}
