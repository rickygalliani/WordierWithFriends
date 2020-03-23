import scala.io.StdIn.readLine

object Main extends App {

  println("WordierWithFriends")

  val board = new Board()

  // val tiles = readLine("What tiles do you have? (separate with commas (i.e., A, B, C))\n")
  //   .split(", ")
  //   .map(c => Tiles.getTile(c))

  // val validWords = (1 to tiles.size).flatMap { length =>
  //   // All subsets of the tiles of the current length
  //   tiles.combinations(length).toList.flatMap { sub => 
  //     // Check all permutations of each tile subset
  //     sub.permutations.toList.flatMap { ord => 
  //       val word = Tiles.getWord(ord)
  //       if (Dictionary.wordIsValid(word)) Option(word) else None
  //     }
  //   }
  // }

  // println(s"\n${validWords.mkString("\n")}")

  board.print()

  val x = board.addWord("TEST", 0, 0)

  board.print()

}
