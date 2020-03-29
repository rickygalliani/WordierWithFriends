import scala.collection.mutable.ListBuffer 

object Word {

  def constructWord(ordering: List[String], fixedLetters: List[(Int, String)]): String = {
    var ordIndex = 0
    var word = new ListBuffer[String]
    // Add ordered letters in between fixed letters
    fixedLetters.foreach { case (letterIndex, letter) =>
      while (word.length < letterIndex) {
        word += ordering(ordIndex)
        ordIndex += 1
      }
      word += letter
    }
    word += ordering.slice(ordIndex, ordering.length).mkString  // Add rest of ordering to word
    word.mkString
  }

  def getWordsWithLength(tiles: Set[Tile],
                         fixedLetters: List[(Int, String)],
                         length: Int): Set[String] = {
    // If letter at length + 1 obstructs construction of word of length, just return
    val letterEnd: String = fixedLetters.filter(_._1 == length).map(_._2).headOption.getOrElse("")
    if (letterEnd.nonEmpty) return Set[String]()

    val fixedLettersWithinLength = fixedLetters.filter(_._1 < length)
    val letters = tiles.map(_.letter).toList
    val tilesToUse = length - fixedLettersWithinLength.length
    // Get all permutations of the given tiles and fixed letters that match this length
    letters.combinations(tilesToUse).toArray.flatMap { sub =>
      sub.permutations.toArray.flatMap { ordering =>
        val word = constructWord(ordering, fixedLettersWithinLength)
        val valWord = Dictionary.wordIsValid(word.mkString)
        if (valWord) Option(word.mkString) else None
      }
    }.toSet
  }

  def getWords(tiles: Set[Tile],
               fixedLetters: List[(Int, String)],
               maxLength: Int): Set[String] = {
    (1 to maxLength).flatMap { len =>
      // Only keep the letters that are fixed within the current length
      getWordsWithLength(tiles, fixedLetters, len)
    }.toSet
  }

}