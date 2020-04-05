import scala.io.StdIn.readLine

import Games._

object Main extends App {

  println("WordierWithFriends")

  val game = Game2  // toggle games here

  val board = new Board()
  game.moves.foreach(move => board.makeMove(move))

  board.print

  // val tiles = readLine("What tiles do you have? (separate with commas (i.e., A, B, C))\n")
  //   .split(", ")
  //   .map(c => Tiles.makeTile(c))
  //   .toSet

  val moves = board.getMoves(game.tiles)
    .map(m => (m, board.getMoveScore(m)))
    .toList
    .sortBy(_._2)(Ordering[Int].reverse)

  moves.foreach(m => println(s"${m._1.asString} ${m._2}"))
}
