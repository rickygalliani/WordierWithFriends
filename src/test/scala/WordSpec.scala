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

  test("getWords(): case 1") {
    val tiles: Set[Tile] = Set(A)
    val fixedLetters: List[(Int, String)] = List[(Int, String)]()
    val maxLength: Int = 10
    val testWords: Set[String] = Word.getWords(tiles, fixedLetters, maxLength)
    assert(testWords == Set("A"))
  }

  test("getWords(): case 2") {
    val tiles: Set[Tile] = Set(A)
    val fixedLetters: List[(Int, String)] = List((0, "A"))
    val maxLength: Int = 10
    val testWords: Set[String] = Word.getWords(tiles, fixedLetters, maxLength)
    assert(testWords == Set("A", "AA"))
  }

  test("getWords(): case 3") {
    val tiles: Set[Tile] = Set(A)
    val fixedLetters: List[(Int, String)] = List((0, "B"))
    val maxLength: Int = 10
    val testWords: Set[String] = Word.getWords(tiles, fixedLetters, maxLength)
    assert(testWords == Set("BA"))
  }

  test("getWords(): case 4") {
    val tiles: Set[Tile] = Set(T)
    val fixedLetters: List[(Int, String)] = List((1, "E"), (2, "S"), (3, "T"))
    val maxLength: Int = 3
    val testWords: Set[String] = Word.getWords(tiles, fixedLetters, maxLength)
    assert(testWords == Set())
  }

  test("getWords(): case 5") {
    val tiles: Set[Tile] = Set(T)
    val fixedLetters: List[(Int, String)] = List((1, "E"), (2, "S"), (3, "T"))
    val maxLength: Int = 6
    val testWords: Set[String] = Word.getWords(tiles, fixedLetters, maxLength)
    assert(testWords == Set("TEST"))
  }

  test("getWords(): case 6") {
    val tiles: Set[Tile] = Set(A)
    val fixedLetters: List[(Int, String)] = List((2, "E"), (3, "S"), (4, "T"))
    val maxLength: Int = 1
    val testWords: Set[String] = Word.getWords(tiles, fixedLetters, maxLength)
    assert(testWords == Set("A"))
  }

  test("constructWord(): case 1") {
    val ord: List[String] = List("C", "V", "E")
    val fixedLetters: List[(Int, String)] = List((1, "A"))
    val length: Int = 2
    assert(Word.constructWord(ord, fixedLetters) == "CAVE")
  }

}