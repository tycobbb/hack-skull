module Cli.View where

-- external
import Prelude hiding (floor)
import qualified Data.List.Split as Split

-- internal
import Utils
import qualified Game as G
import qualified Game.Vec as V
import qualified Game.World as W

{- types -}
data Tile = Tile
  { glyph :: Char
  , pos   :: V.Vec2
  }

{- values -}
floor :: V.Vec2 -> Tile
floor = Tile '.'

human :: V.Vec2 -> Tile
human = Tile '@'

{- impls -}
-- Renders the game to string
--
-- @param game The game to render
render :: G.Game -> IO ()
render game = do
  -- reset screen
  putStr "\ESC[H\ESC[2J"
  -- render world
  (game#G.world)
    |> renderWorld
    |> putStr

-- Renders the world to string
--
-- @param world The world to render
renderWorld :: W.World -> String
renderWorld world =
  drawGround (world#W.size)
    |> addLayer (drawPlayer (world#W.player))
    |> map glyph
    |> Split.chunksOf (world#W.size#V.x)
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

-- Draws the ground
drawGround :: V.Vec2 -> [Tile]
drawGround size =
  drawGround' [] size

drawGround' :: [Tile] -> V.Vec2 -> [Tile]
drawGround' memo pos
  | V.y pos == 1 = drawGroundRow (pos - V.uy) ++ memo
  | otherwise    = drawGround' ((drawGroundRow (pos - V.uy)) ++ memo) (pos - V.uy)

-- Draws a ground row
drawGroundRow :: V.Vec2 -> [Tile]
drawGroundRow =
  drawGroundRow' []

drawGroundRow' :: [Tile] -> V.Vec2 -> [Tile]
drawGroundRow' memo pos
  | V.x pos == 1 = (floor (pos - V.ux)) : memo
  | otherwise    = drawGroundRow' ((floor (pos - V.ux)) : memo) (pos - V.ux)

-- Draws a player
drawPlayer :: W.Actor -> [Tile]
drawPlayer player =
  [human (player#W.pos)]
