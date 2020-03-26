class Position(val letterFactor: Int = 1, val wordFactor: Int = 1) {

  def canEqual(a: Any) = a.isInstanceOf[Position] 

  override def equals(that: Any): Boolean = {
    that match { 
      case that: Position => {
        that.canEqual(this) &&
        that.letterFactor == this.letterFactor &&
        that.wordFactor == this.wordFactor &&
        that.getTile == this.getTile
      }
      case _ => false
    } 
  }

  private var tile: Option[Tile] = None

  def getTile: Option[Tile] = tile
  def setTile(newTile: Tile): Unit = tile = Option(newTile)
  def isOpen(): Boolean = !tile.isDefined
  
}

class RegularPosition extends Position()
class DoubleLetterPosition extends Position(2, 1)
class TripleLetterPosition extends Position(3, 1)
class DoubleWordPosition extends Position(1, 2)
class TripleWordPosition extends Position(1, 3)

object Position {

  def rp: RegularPosition = new RegularPosition
  def dl: DoubleLetterPosition = new DoubleLetterPosition
  def tl: TripleLetterPosition = new TripleLetterPosition
  def dw: DoubleWordPosition = new DoubleWordPosition
  def tw: TripleWordPosition = new TripleWordPosition

}