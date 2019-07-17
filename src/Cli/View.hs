module Cli.View where

-- external
import Prelude hiding (floor)
import qualified Data.List.Split as Split

-- internal
import Core.Utils
import qualified Game as G
import Game (Game)
import qualified Game.Vec as V
import Game.Vec (Vec2)
import qualified Game.World as W
import Game.World (World, Actor)
import qualified Game.Level as L
import Game.Level (Level)

{- types -}
data Tile = Tile
  { glyph :: Char
  , pos   :: Vec2
  }

{- values -}
empty :: Vec2 -> Tile
empty = Tile ' '

floor :: Vec2 -> Tile
floor = Tile '.'

human :: Vec2 -> Tile
human = Tile '@'

{- impls -}
{- impls/commands -}
-- Resets the screen.
reset :: IO ()
reset =
  putStr "\ESC[H\ESC[2J"

-- Renders the game to string.
--
-- @param game The game to render
render :: Game -> IO ()
render game = do
  -- reset screen
  reset
  -- render world
  (game#G.world)
    |> renderWorld
    |> putStr

-- Renders the world to string
--
-- @param world The world to render
renderWorld :: World -> String
renderWorld world =
  drawGround (world#W.level)
    |> addLayer (drawPlayer (world#W.player))
    |> map glyph
    |> Split.chunksOf (world#W.width)
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
drawGround :: Level -> [Tile]
drawGround level =
  drawGround' level (level#L.size) []

drawGround' :: Level -> Vec2 -> [Tile] -> [Tile]
drawGround' level pos memo
  | V.y pos == 1 = drawGroundRow level (pos - V.uy) ++ memo
  | otherwise    = drawGround' level (pos - V.uy) ((drawGroundRow level (pos - V.uy)) ++ memo)

-- Draws a ground row
drawGroundRow :: Level -> Vec2 -> [Tile]
drawGroundRow level pos =
  drawGroundRow' level pos []

drawGroundRow' :: Level -> Vec2 -> [Tile] -> [Tile]
drawGroundRow' level pos memo
  | V.x pos == 1 = (drawGroundTile level (pos - V.ux)) : memo
  | otherwise    = drawGroundRow' level (pos - V.ux) ((drawGroundTile level (pos - V.ux)) : memo)

drawGroundTile :: Level -> Vec2 -> Tile
drawGroundTile level pos =
  if L.cell level pos then
    floor pos
  else
    empty pos

-- Draws a player
drawPlayer :: Actor -> [Tile]
drawPlayer player =
  [human (player#W.pos)]
