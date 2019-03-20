module World where

{- types -}

-- The game world.
data World = World {
  room   :: [((Int, Int), Char)],
  player :: [((Int, Int), Char)]
}
