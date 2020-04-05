class Game(val moves: Set[Move], val tiles: Set[Tile])

object Games {
  case object Game1 extends Game(
    Set(
      Move("FUNDS", -2, 0, Move.Horizontal),
      Move("FIX", -3, 1, Move.Horizontal),
      Move("POSER", 2, 2, Move.Vertical),
      Move("EVIL", 2, -1, Move.Horizontal),
      Move("NIPS", 4, 0, Move.Vertical),
      Move("PORN", 4, -2, Move.Horizontal),
      Move("GNAT", 7, -1, Move.Vertical),
      Move("WALER", -2, -2, Move.Horizontal),
      Move("FIX", -3, 1, Move.Horizontal),
      Move("RAH", -1, -3, Move.Horizontal),
      Move("LATHE", 0, -2, Move.Vertical),
      Move("DEUCE", -1, -6, Move.Horizontal),
      Move("BE", -2, -7, Move.Horizontal),
      Move("KIBE", -4, -7, Move.Horizontal),
      Move("BEG", 3, -5, Move.Horizontal),
      Move("PEARL", 2, 2, Move.Horizontal),
      Move("DEITY", 3, -7, Move.Horizontal),
      Move("TED", 3, 3, Move.Vertical)
    ), 
    Set(Z, G, E, E, L, N, A)
  )

  case object Game2 extends Game(
    Set(
      Move("JOL", -3, 1, Move.Horizontal),
      Move("ROAR", -2, 0, Move.Horizontal),
      Move("XI", -1, -1, Move.Horizontal),
      Move("AIMED", 0, 0, Move.Vertical),
      Move("LEANEST", -1, -3, Move.Horizontal),
      Move("REEL", 3, -2, Move.Vertical),
      Move("SOOKS", 4, -3, Move.Vertical),
      Move("GLOBED", 2, -5, Move.Horizontal),
      Move("WO", 6, -4, Move.Horizontal),
      Move("FROGS", 0, -7, Move.Horizontal)
      Move("MEN", 2, -1, Move.Vertical)
    ), 
    Set(Q, M, T, N, T, E, E)
  )

}
