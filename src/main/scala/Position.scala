class Position(val letterFactor: Int = 1, val wordFactor: Int = 1) {

  private var tile: Option[Tile] = None

  def getTile: Option[Tile] = tile
  def setTile(newTile: Tile): Unit = tile = Option(newTile)
  def clearTile(): Unit = tile = None
  
}

class RegularPosition extends Position()  // Regular position
class DoubleLetterPosition extends Position(2, 1)  // Double letter
class TripleLetterPosition extends Position(3, 1)  // Triple letter
class DoubleWordPosition extends Position(1, 2)  // Double word
class TripleWordPosition extends Position(1, 3)  // Triple word

object Position {

	def rp: RegularPosition = new RegularPosition
	def dl: DoubleLetterPosition = new DoubleLetterPosition
	def tl: TripleLetterPosition = new TripleLetterPosition
	def dw: DoubleWordPosition = new DoubleWordPosition
	def tw: TripleWordPosition = new TripleWordPosition

}