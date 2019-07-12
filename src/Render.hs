module Render where

-- external
import Prelude hiding (floor)
import qualified Data.List.Split as Split

-- internal
import Utils
import qualified Vec as V
import qualified World as W

{- types -}
data Tile = Tile {
  glyph :: Char,
  pos   :: V.Vec2
}

{- values -}
hwall :: V.Vec2 -> Tile
hwall = Tile '-'

vwall :: V.Vec2 -> Tile
vwall = Tile '|'

floor :: V.Vec2 -> Tile
floor = Tile '.'

human :: V.Vec2 -> Tile
human = Tile '@'

{- impls -}
-- Renders a world to string
--
-- @param world The world to render
render :: W.World -> String
render world =
  drawRoom (W.room world)
    |> addLayer (drawPlayer (W.player world))
    |> map glyph
    |> Split.chunksOf 4
    |> unlines

-- Merges two tile layers. The top tile layer will overwrite any tiles at a
-- matching position in the bottom layer.
--
-- @param top    The top tile layer; may be sparse
-- @param bottom The bottom tile layer; must _not_ be sparse
-- 
-- @return A merged tile layer
addLayer :: [Tile] -> [Tile] -> [Tile]
addLayer [] [] = []
addLayer [] (b1 : bot) = b1 : addLayer [] bot
addLayer (t1 : top) [] = t1 : addLayer top []
addLayer (t1 : top) (b1 : bot)
  | pos t1 == pos b1 = t1 : addLayer top bot
  | null top         = b1 : addLayer [t1] bot
  | otherwise        = b1 : addLayer top bot

-- Draws a room
drawRoom :: W.Room -> [Tile]
drawRoom room =
  drawRoom' [] (W.bounds room)

drawRoom' :: [Tile] -> V.Vec2 -> [Tile]
drawRoom' memo pos
  -- top
  | V.y pos == 1 = (drawRoomExterior (V.setY 0 pos)) ++ memo
  -- bottom
  | null memo    = drawRoom' (drawRoomExterior (pos - V.uy)) (pos - V.uy)
  -- middle
  | otherwise    = drawRoom' ((drawRoomInterior (pos - V.uy)) ++ memo) (pos - V.uy)

-- Draws a room's top/bottom rows
drawRoomExterior :: V.Vec2 -> [Tile]
drawRoomExterior pos =
  [0..(V.x pos)-1]
    |> map (\x -> hwall (V.setX x pos))

-- Draws a room's inner rows
drawRoomInterior :: V.Vec2 -> [Tile]
drawRoomInterior =
  drawRoomInterior' []

drawRoomInterior' :: [Tile] -> V.Vec2 -> [Tile]
drawRoomInterior' memo pos
  -- left
  | V.x pos == 1 = (vwall (V.setX 0 pos)) : memo
  -- right
  | null memo    = drawRoomInterior' [vwall (pos - V.ux)] (pos - V.ux)
  -- center
  | otherwise    = drawRoomInterior' ((floor (pos - V.ux)) : memo) (pos - V.ux)

-- Draws a player
drawPlayer :: W.Actor -> [Tile]
drawPlayer player =
  [human (W.pos player)]
