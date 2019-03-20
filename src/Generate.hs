module Generate where

import qualified World

-- generates the world
world :: World.World
world = World.World {
  World.room   = room 4 4,
  World.player = [((1, 1), '@')]
}

-- generates a room given width and height
room :: Int -> Int -> [((Int, Int), Char)]
room =
  room' []

-- generates a room (recursively)
room' :: [((Int, Int), Char)] -> Int -> Int -> [((Int, Int), Char)]
room' memo width height
  -- top
  | height == 1  = (exterior width 0) ++ memo
  -- bottom
  | memo == []   = room' (exterior width (height - 1)) width (height - 1)
  -- center
  | otherwise    = room' ((interior width (height - 1)) ++ memo) width (height - 1)

-- generates an exterior (top/bottom) row
exterior :: Int -> Int -> [((Int, Int), Char)]
exterior width y =
  map (\x -> ((x, y), '-')) [0..width-1]

-- generates an interior row
interior :: Int -> Int -> [((Int, Int), Char)]
interior =
  interior' []

-- generates an interior row (recursively)
interior' :: [((Int, Int), Char)] -> Int -> Int -> [((Int, Int), Char)]
interior' memo width y
  -- left
  | width == 1  = (((0, y), '|') : memo)
  -- right
  | memo == []  = interior' [((width - 1, y), '|')] (width - 1) y
  -- center
  | otherwise   = interior' (((width - 1, y), '.') : memo) (width - 1) y
