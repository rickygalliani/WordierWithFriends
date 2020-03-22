class Tile(val letter: String, val points: Int)

case object A extends Tile("A", 1)
case object B extends Tile("B", 4)
case object C extends Tile("C", 4)
case object D extends Tile("D", 2)
case object E extends Tile("E", 1)
case object F extends Tile("F", 4)
case object G extends Tile("G", 3)
case object H extends Tile("H", 3)
case object I extends Tile("I", 1)
case object J extends Tile("J", 10)
case object K extends Tile("K", 5)
case object L extends Tile("L", 2)
case object M extends Tile("M", 4)
case object N extends Tile("N", 2)
case object O extends Tile("O", 1)
case object P extends Tile("P", 4)
case object Q extends Tile("Q", 10)
case object R extends Tile("R", 1)
case object S extends Tile("S", 1)
case object T extends Tile("T", 1)
case object U extends Tile("U", 2)
case object V extends Tile("V", 5)
case object W extends Tile("W", 4)
case object X extends Tile("X", 8)
case object Y extends Tile("Y", 3)
case object Z extends Tile("Z", 10)

object Tiles {

  private val TileMap: Map[String, Tile] = Map(
    "A" -> A,
    "B" -> B,
    "C" -> C,
    "D" -> D,
    "E" -> E,
    "F" -> F,
    "G" -> G,
    "H" -> H,
    "I" -> I,
    "J" -> J,
    "K" -> K,
    "L" -> L,
    "M" -> M,
    "N" -> N,
    "O" -> O,
    "P" -> P,
    "Q" -> Q,
    "R" -> R,
    "S" -> S,
    "T" -> T,
    "U" -> U,
    "V" -> V,
    "W" -> W,
    "X" -> X,
    "Y" -> Y,
    "Z" -> Z
  )

  def getTile(letter: String): Tile = TileMap(letter)

  def getWord(tiles: Array[Tile]): String = tiles.map(_.letter).mkString("")

}