import scala.collection.mutable.HashSet 

object Dictionary {

  private def load(): HashSet[String] = {
    var dictionary = HashSet[String]() 
    var index = 0
    val dictionaryFile = "/Users/pjgalliani/Code/WordierWithFriends/dictionary.csv"
    val bufferedSource = io.Source.fromFile(dictionaryFile).getLines
    for (word <- bufferedSource) dictionary += word
    dictionary
  }

  private val Dictionary = load()

  def wordIsValid(word: String): Boolean = Dictionary.contains(word)

}