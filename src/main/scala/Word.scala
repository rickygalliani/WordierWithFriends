object Word {

  def getWordsForLength(tiles: Set[Tile],
                        length: Int,
                        prefix: String = "",
                        suffix: String = ""): Set[String] = {
    val numAlreadyFixed = prefix.length + suffix.length
    if (numAlreadyFixed > length) throw new IllegalArgumentException("Invalid prefix or suffix")
    val validWords = {
      // All subsets of the tiles of the current length
      tiles.toArray.combinations(length - numAlreadyFixed).toList.flatMap { sub => 
        // Check all permutations of each tile subset
        sub.permutations.toArray.flatMap { ord => 
          val word = s"${prefix}${Tiles.getWord(ord)}${suffix}"
          if (Dictionary.wordIsValid(word)) Option(word) else None
        }
      }
    }
    validWords.toSet
  }

  def getWords(tiles: Set[Tile],
               lengths: Set[Int],
               prefix: String = "",
               suffix: String = ""): Set[String] = {
    lengths.flatMap(len => getWordsForLength(tiles, len, prefix, suffix))
  }

}