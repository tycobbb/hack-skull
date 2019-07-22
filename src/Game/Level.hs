module Game.Level where

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
import qualified Game.Cell as C
import Game.Cell (Cell(..), Room(..))

{- types -}
data Level = Level
  { size :: Vec2
  , grid :: Vector Cell
  }

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
init gen size =
  seedR gen size
    |> repeatStepR 15

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
    |> R.map (foldr (seedCell 1) ([], Room 0))
    |> R.map (Vector.fromList . Tuple.fst)
    |> R.map (Level size)

seedCell :: Int -> Int -> ([Cell], Room) -> ([Cell], Room)
seedCell threshold sample (cells, room) =
  if sample < threshold then
    (Floor room : cells, Room (C.roomId room + 1))
  else
    (Empty : cells, room)

{- impls/gen/step -}
repeatStepR :: Int -> Rand Level -> Rand Level
repeatStepR n value =
  if n == 0 then
    value
  else
    repeatStepR (n - 1) (stepR value)

stepR :: Rand Level -> Rand Level
stepR level =
  level
    |> stepGridR
    |> R.map (\grid -> (Tuple.fst level) { grid = grid })

stepGridR :: Rand Level -> Rand (Vector Cell)
stepGridR (level, gen) =
  (level#grid)
    |> Vector.ifoldr (\i cell cells ->
      cells
        |> R.join cell (:) (stepCellR level i)) ([], gen)
    |> R.map Vector.fromList

stepCellR :: Level -> Int -> Rand Cell -> Rand Cell
stepCellR level i (cell, gen) =
  let
    pos =
      V.fromIndex (level#size) i
    neighbors =
      findNeighbors level pos
    numberOfNeighbors =
      countNeighbors neighbors
  in
    if C.isOn cell then
      (cell, gen)
    else
      R.random (0, 100) gen
        |> R.map (stepCell (15 * numberOfNeighbors) (Room 0))

stepCell :: Int -> Room -> Int -> Cell
stepCell threshold room sample =
  if sample < threshold then
    Floor room
  else
    Empty

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
    |> filter (\neighbor -> C.isOn (cell neighbor) )
    |> List.length
