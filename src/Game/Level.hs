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
import Core.Rand (Rand)
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
-- @param size The bounding size of the level
init :: Vec2 -> Rand Level
init size =
  seed size
    >>= stepN 15

{- impls/queries -}
-- Gets the cell at the index
celli :: Level -> Int -> Cell
celli level i =
  (level#grid) Vector.! i

-- Gets the cell at the position
cellv :: Level -> Vec2 -> Cell
cellv level pos =
  celli level (V.toIndex (level#size) pos)

{- impls/setters -}
setGrid :: Level -> Vector Cell -> Level
setGrid level grid =
  level { grid = grid }

-- Transforms each cell in the level.
--
-- @param fn A transform that receives the index and cell.
imap :: (Int -> Cell -> a) -> Level -> [a]
imap fn level =
  (level#grid)
    |> Vector.ifoldr (\i cell list -> fn i cell : list) []

{- impls/gen -}
{- impls/gen/seed -}
seed :: Vec2 -> Rand Level
seed size =
  R.sampleN options (V.mag size)
    |> fmap Vector.fromList
    |> fmap (Level size)
  where
    options =
      [ (1,  Floor (Room 0))
      , (99, Empty)
      ]

{- impls/gen/step -}
stepN :: Int -> Level -> Rand Level
stepN n level =
  if n == 0 then
    pure level
  else
    step level
      >>= stepN (n - 1)

step :: Level -> Rand Level
step level =
  level
    |> imap (stepCell level)
    |> sequenceA
    |> fmap Vector.fromList
    |> fmap (setGrid level)

stepCell :: Level -> Int -> Cell -> Rand Cell
stepCell level i cell =
  cell
    |> C.thenA (R.sample options)
  where
    floorChance =
      V.fromIndex (level#size) i
        |> findNeighbors level
        |> countNeighbors
        |> (*15)
    options =
      [ (floorChance,       Floor (Room 0))
      , (100 - floorChance, Empty)
      ]

{- impls/gen/neighbors -}
findNeighbors :: Level -> Vec2 -> [Neighbor]
findNeighbors level pos =
  let
    findNeighbor' =
      findNeighbor level pos
  in
    Maybe.catMaybes
      [ findNeighbor' Up
      , findNeighbor' Down
      , findNeighbor' Left
      , findNeighbor' Right
      ]

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
    |> filter (not . C.isEmpty . cell)
    |> List.length
