module Render where

-- external
import qualified Data.List.Split as Split

-- internal
import World (World)
import qualified World

{- impls -}

-- Renders a world to string
--
-- @param world The world to render
render :: World -> String
render world =
  unlines (Split.chunksOf 4 (map snd (merge (World.player world) (World.room world))))

-- Merges two tile layers. The top tile layer will overwrite any tiles at a
-- matching position in the bottom layer.
--
-- @param top    The top tile layer; may be sparse
-- @param bottom The bottom tile layer; must _not_ be sparse
merge :: [((Int, Int), Char)] -> [((Int, Int), Char)] -> [((Int, Int), Char)]
merge [] [] = []
merge [] (b1 : bot) = b1 : merge [] bot
merge (t1 : top) [] = t1 : merge top []
merge (t1 : top) (b2 : bot)
  | fst t1 == fst b2 = t1 : merge top bot
  | top == []        = b2 : merge [t1] bot
  | otherwise        = b2 : merge top bot
