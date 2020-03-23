import scala.io.StdIn.readLine

object Main extends App {

  println("WordierWithFriends")

  val board = new Board()

  board.makeMove(Move("OATH", -1, 0, Move.Horizontal))
  board.makeMove(Move("INANE", 0, 2, Move.Vertical))
  board.makeMove(Move("CHURN", -4, -1, Move.Horizontal))
  board.makeMove(Move("HOARD", 2, 0, Move.Vertical))
  board.makeMove(Move("OAT", 2, -1, Move.Horizontal))
  board.makeMove(Move("DETANGLED", 4, 1, Move.Vertical))
  board.makeMove(Move("VIBE", 6, 1, Move.Vertical))
  board.makeMove(Move("AGAPES", 2, -2, Move.Horizontal))
  board.makeMove(Move("RAN", 2, -3, Move.Horizontal))
  board.makeMove(Move("WEED", -1, -4, Move.Horizontal))
  board.makeMove(Move("MWAH", -1, -3, Move.Vertical))
  board.makeMove(Move("COOLING", -4, -1, Move.Vertical))
  board.makeMove(Move("YAM", -6, 1, Move.Horizontal))
  board.makeMove(Move("YEAH", -6, 1, Move.Vertical))
  board.makeMove(Move("BEEFS", -7, -1, Move.Vertical))
  board.makeMove(Move("FOIL", -7, -4, Move.Horizontal))
  board.makeMove(Move("HOTE", -1, -6, Move.Horizontal))
  board.makeMove(Move("SPITES", 7, -2, Move.Vertical))

  board.print

  // val tiles = readLine("What tiles do you have? (separate with commas (i.e., A, B, C))\n")
  //   .split(", ")
  //   .map(c => Tiles.makeTile(c))
  //   .toSet

  val tiles = Seq("U", "L", "E", "I", "W", "D", "U").map(c => Tiles.makeTile(c)).toSet

  val moves = board.getMoves(tiles)
    .map(m => (m, board.getMoveScore(m)))
    .toList
    .sortBy(_._2)(Ordering[Int].reverse)
  moves.foreach(m => println(s"${m._1.asString} ${m._2}"))

}
