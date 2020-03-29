object Word {

  // def getWordsForLength(tiles: Set[Tile],
  //                       length: Int,
  //                       prefix: String = "",
  //                       suffix: String = ""): Set[String] = {
  //   val numAlreadyFixed = prefix.length + suffix.length
  //   if (numAlreadyFixed > length) throw new IllegalArgumentException("Invalid prefix or suffix")
  //   val validWords = {
  //     // All subsets of the tiles of the current length
  //     tiles.toArray.combinations(length - numAlreadyFixed).toList.flatMap { sub => 
  //       // Check all permutations of each tile subset
  //       sub.permutations.toArray.flatMap { ord => 
  //         val word = s"${prefix}${Tiles.getWord(ord)}${suffix}"
  //         if (Dictionary.wordIsValid(word)) Option(word) else None
  //       }
  //     }
  //   }
  //   validWords.toSet
  // }

  // def getWords(tiles: Set[Tile],
  //              lengths: Set[Int],
  //              prefix: String = "",
  //              suffix: String = ""): Set[String] = {
  //   lengths.flatMap(len => getWordsForLength(tiles, len, prefix, suffix))
  // }

  def getWordsWithLength(tiles: Set[Tile],
                            fixedLetters: List[(Int, String)],
                            length: Int): Set[String] = {
    val allLetters = (tiles.map(_.letter).toList ++ fixedLetters.map(_._2)).toArray
    // Get all permutations of the given tiles and fixed letters that match this length
    allLetters.combinations(length).toArray.flatMap { sub =>
      sub.permutations.toArray.flatMap { word =>
        // Ignore permutations that don't have the same fixed letters
        val valOrdering = fixedLetters.map { case (ind, let) => word(ind) == let }.forall(identity)
        val valWord = Dictionary.wordIsValid(word.mkString)
        if (valOrdering && valWord) Option(word.mkString) else None
      }
    }.toSet
  }

  def getWords(tiles: Set[Tile],
               fixedLetters: List[(Int, String)],
               maxLength: Int): Set[String] = {
    (1 to maxLength).flatMap { len =>
      // Only keep the letters that are fixed within the current length
      getWordsWithLength(tiles, fixedLetters.filter(_._1 < len), len)
    }.toSet
  }

}