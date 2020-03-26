import org.scalatest.FunSuite

class WordSpec extends FunSuite {

  test("getWordsWithLength(): case 1") {
    
    val tiles: Set[Tile] = Set(Z)
    val fixedLetters: List[(Int, String)] = List((1, "O"), (2, "O"))
    val length: Int = 3
    val testWords: Set[String] = Word.getWordsWithLength(tiles, fixedLetters, length)
    assert(testWords == Set("ZOO"))

  }

  test("getWordsWithLength(): case 2") {
    
    val tiles: Set[Tile] = Set(Z, T, B, M, P, A, X)
    val fixedLetters: List[(Int, String)] = List((1, "O"), (2, "O"))
    val length: Int = 3
    val testWords: Set[String] = Word.getWordsWithLength(tiles, fixedLetters, length)
    assert(testWords == Set("ZOO", "TOO", "BOO", "MOO", "POO"))

  }

  test("getWordsWithLength(): case 3") {
    
    val tiles: Set[Tile] = Set(Z)
    val fixedLetters: List[(Int, String)] = List((1, "O"), (2, "O"))
    val length: Int = 5
    val testWords: Set[String] = Word.getWordsWithLength(tiles, fixedLetters, length)
    assert(testWords == Set())

  }

}