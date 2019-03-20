module Lib where

import Debug.Trace
import qualified Data.List.Split as Split

root :: String
root =
  render world

-- ----- --
-- world --
-- ----- --
data World = World {
  mRoom   :: [((Int, Int), Char)],
  mPlayer :: [((Int, Int), Char)]
}

world = World {
  mRoom   = room 4 4,
  mPlayer = [((1, 1), '@')]
}

-- ---------- --
-- generation --
-- ---------- --

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

-- --------- --
-- rendering --
-- --------- --

render :: World -> String
render world =
  unlines (Split.chunksOf 4 (map snd (merge (mPlayer world) (mRoom world))))

merge :: [((Int, Int), Char)] -> [((Int, Int), Char)] -> [((Int, Int), Char)]
merge [] [] = []
merge [] (b1 : bot) = b1 : merge [] bot
merge (t1 : top) [] = t1 : merge top []
merge (t1 : top) (b2 : bot)
  | fst t1 == fst b2 = t1 : merge top bot
  | top == []        = b2 : merge [t1] bot
  | otherwise        = b2 : merge top bot
