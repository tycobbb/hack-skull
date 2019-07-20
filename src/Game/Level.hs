module Game.Level where

-- external
import qualified Data.Tuple as Tuple
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
  case cell of
    On  -> (On, gen)
    Off -> if (neighborsi level i) >= 1 then (On, gen) else (Off, gen)

{- impls/gen/neighbors -}
neighborsi :: Level -> Int -> Int
neighborsi level i =
  let
    pos =
      V.fromIndex (level#size) i
    isOn pos =
      V.contains pos (level#size) && (cellv level pos) == On
    count pos =
      if isOn pos then 1 else 0
  in
    count (pos + V.ux) +
    count (pos - V.ux) +
    count (pos + V.uy) +
    count (pos - V.uy)
