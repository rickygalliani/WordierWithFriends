import scala.io.StdIn.readLine

object Main extends App {

  println("WordierWithFriends")

  val board = new Board()

  board.makeMove(Move("FUNDS", -2, 0, Move.Horizontal))
  board.makeMove(Move("FIX", -3, 1, Move.Horizontal))
  board.makeMove(Move("POSER", 2, 2, Move.Vertical))
  board.makeMove(Move("EVIL", 2, -1, Move.Horizontal))
  board.makeMove(Move("NIPS", 4, 0, Move.Vertical))
  board.makeMove(Move("PORN", 4, -2, Move.Horizontal))
  board.makeMove(Move("GNAT", 7, -1, Move.Vertical))
  board.makeMove(Move("WALER", -2, -2, Move.Horizontal))
  board.makeMove(Move("FIX", -3, 1, Move.Horizontal))
  board.makeMove(Move("RAH", -1, -3, Move.Horizontal))
  board.makeMove(Move("LATHE", 0, -2, Move.Vertical))
  board.makeMove(Move("DEUCE", -1, -6, Move.Horizontal))
  board.makeMove(Move("BE", -2, -7, Move.Horizontal))
  board.makeMove(Move("KIBE", -4, -7, Move.Horizontal))
  board.makeMove(Move("BEG", 3, -5, Move.Horizontal))
  board.makeMove(Move("PEARL", 2, 2, Move.Horizontal))
  board.makeMove(Move("DEITY", 3, -7, Move.Horizontal))

  board.print

  // val tiles = readLine("What tiles do you have? (separate with commas (i.e., A, B, C))\n")
  //   .split(", ")
  //   .map(c => Tiles.makeTile(c))
  //   .toSet

  val tiles = Seq("Z", "G", "T", "E", "E", "L", "D").map(c => Tiles.makeTile(c)).toSet

  val moves = board.getMoves(tiles)
    .map(m => (m, board.getMoveScore(m)))
    .toList
    .sortBy(_._2)(Ordering[Int].reverse)

  moves.foreach(m => println(s"${m._1.asString} ${m._2}"))
}
