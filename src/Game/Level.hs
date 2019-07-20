module Game.Level where

import Debug.Trace

-- external
import Prelude hiding (Left, Right)
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple
import qualified Data.List as List
import qualified Data.Vector as Vector
import Data.Vector (Vector)

-- internal
import Core.Utils
import qualified Core.Rand as R
import Core.Rand (Rand, RandGen)
import qualified Game.Vec as V
import Game.Vec (Vec2)

{- types -}
data Level = Level
  { size :: Vec2
  , grid :: Vector Cell
  }

data Cell
  = On
  | Off
  deriving (Eq, Show)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Show)

data Neighbor = Neighbor
  { cell :: Cell
  , dir  :: Direction
  }
  deriving (Show)

{- impls -}
-- Initializes a level.
--
-- @param gen  A random generator
-- @param size The bounding size of the level
--
-- @return A new level
init :: RandGen -> Vec2 -> Rand Level
init =
  seedR

{- impls/queries -}
-- Gets the cell at the index
celli :: Level -> Int -> Cell
celli level i =
  (level#grid) Vector.! i

-- Gets the cell at the position
cellv :: Level -> Vec2 -> Cell
cellv level pos =
  celli level (V.toIndex (level#size) pos)

-- Transforms each cell in the level.
--
-- @param fn A transform that receives the index and cell.
imap :: (Int -> Cell -> a) -> Level -> [a]
imap fn level =
  (level#grid)
    |> Vector.ifoldr (\i cell list -> fn i cell : list) []

{- impls/gen -}
{- impls/gen/seed -}
seedR :: RandGen -> Vec2 -> Rand Level
seedR gen size =
  R.generate (0, 100) gen (V.mag size)
    |> R.map (map (seedCell 5)) -- 5%
    |> R.map Vector.fromList
    |> R.map (Level size)

seedCell :: Int -> Int -> Cell
seedCell threshold sample =
  if sample < threshold then On else Off

{- impls/gen/step -}
stepR :: Rand Level -> Rand Level
stepR level =
  level
    |> stepGridR
    |> R.map (\grid -> (Tuple.fst level) { grid = grid })

stepGridR :: Rand Level -> Rand (Vector Cell)
stepGridR (level, gen) =
  (level#grid)
    |> Vector.ifoldr (\i cell (cells, gen) ->
      stepCellR level i (cell, gen)
        |> R.map (\cell -> cell : cells)) ([], gen)
    |> R.map Vector.fromList

stepCellR :: Level -> Int -> Rand Cell -> Rand Cell
stepCellR level i (cell, gen) =
  let
    pos =
      V.fromIndex (level#size) i
    neighbors =
      findNeighbors level pos
  in
    case cell of
      On  -> (On, gen)
      Off -> if (countNeighbors neighbors) >= 1 then (On, gen) else (Off, gen)

{- impls/gen/neighbors -}
findNeighbors :: Level -> Vec2 -> [Neighbor]
findNeighbors level pos =
  let
    findNeighbor' =
      findNeighbor level pos
  in
    [ findNeighbor' Up
    , findNeighbor' Down
    , findNeighbor' Left
    , findNeighbor' Right
    ]
    |> Maybe.catMaybes

findNeighbor :: Level -> Vec2 -> Direction -> Maybe Neighbor
findNeighbor level origin dir =
  let
    pos =
      findNeighborPos origin dir
  in
    if not (V.contains pos (level#size)) then
      Nothing
    else
      Just Neighbor
        { cell = (cellv level pos)
        , dir  = dir
        }

findNeighborPos :: Vec2 -> Direction -> Vec2
findNeighborPos origin dir =
  case dir :: Direction of
    Up    -> origin + V.uy
    Down  -> origin - V.uy
    Left  -> origin - V.ux
    Right -> origin + V.ux

countNeighbors :: [Neighbor] -> Int
countNeighbors neighbors =
  neighbors
    |> filter (\neighbor -> cell neighbor == On )
    |> List.length
