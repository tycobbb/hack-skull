module Cli.View where

-- external
import Prelude hiding (floor)
import qualified Data.Char as Char
import qualified Data.List.Split as Split

-- internal
import Core.Extra
import qualified Game as G
import Game (Game)
import qualified Game.Vec as V
import Game.Vec (Vec2)
import qualified Game.World as W
import Game.World (World, Actor)
import qualified Game.Level as L
import qualified Game.Level.Grid as LG
import Game.Level (Level)
import qualified Game.Level.Cell as C
import Game.Level.Cell (Cell(..), Room(..))
import qualified Cli.Config as Cfg
import Cli.Config (Config)

{- types -}
data Tile = Tile
  { glyph :: Char
  , pos   :: Vec2
  }

{- impls -}
{- impls/commands -}
-- Resets the screen.
reset :: IO ()
reset =
  putStr "\ESC[H\ESC[2J"

-- Renders the game to string.
--
-- @param game The game to render
render :: Config -> Game -> IO ()
render cfg game = do
  -- reset screen
  reset
  -- render world
  (game#G.world)
    |> renderWorld cfg
    |> putStr

-- Renders the world to string
--
-- @param world The world to render
renderWorld :: Config -> World -> String
renderWorld cfg world =
  drawGround cfg (world#W.level)
    |> addLayer (drawPlayer cfg (world#W.player))
    |> map glyph
    |> Split.chunksOf (world#W.level#LG.width)
    |> unlines

-- Merges two tile layers. The top tile layer will overwrite any tiles at a
-- matching position in the bottom layer.
--
-- @param top    The top tile layer; may be sparse
-- @param bottom The bottom tile layer; must _not_ be sparse
--
-- @return A merged tile layer
addLayer :: [Tile] -> [Tile] -> [Tile]
addLayer [] [] =
  []
addLayer [] (b1 : bot) =
  b1 : addLayer [] bot
addLayer (t1 : top) [] =
  t1 : addLayer top []
addLayer (t1 : top) (b1 : bot)
  | pos t1 == pos b1 = t1 : addLayer top bot
  | null top         = b1 : addLayer [t1] bot
  | otherwise        = b1 : addLayer top bot

-- Draws the ground
drawGround :: Config -> Level -> [Tile]
drawGround cfg level =
  level
    |> LG.imap (drawGroundTile cfg level)

drawGroundTile :: Config -> Level -> Int -> Cell -> Tile
drawGroundTile cfg level i cell =
  let
    pos =
      LG.pos level i
  in
    case cell of
      Floor r -> floor cfg r pos
      Empty   -> empty cfg pos

-- Draws a player
drawPlayer :: Config -> Actor -> [Tile]
drawPlayer cfg player =
  [ human cfg (player#W.pos)
  ]

{- impls/tiles -}
empty :: Config -> Vec2 -> Tile
empty _ =
  Tile ' '

floor :: Config -> Room -> Vec2 -> Tile
floor cfg (Room roomId) =
  if Cfg.debug cfg then
    Tile (Char.intToDigit roomId)
  else
    Tile '.'

human :: Config -> Vec2 -> Tile
human _ =
  Tile '@'
