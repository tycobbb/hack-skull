module Game.Level.Grid where

-- external
import qualified Data.Vector as Vector
import Data.Vector (Vector)

-- internal
import Core.Utils
import qualified Game.Vec as V
import Game.Vec (Vec2)
import qualified Game.Level.Cell as C
import Game.Level.Cell (Cell)

{- types -}
data Grid = Grid
  { size  :: Vec2
  , cells :: Vector Cell
  }

{- impls -}
{- impls/queries -}
-- Gets the position from a flat index
pos :: Grid -> Int -> Vec2
pos grid =
  V.fromIndex (grid#size)

-- Gets the cell at the index
celli :: Grid -> Int -> Cell
celli grid i =
  (grid#cells) Vector.! i

-- Gets the cell at the position
cellv :: Grid -> Vec2 -> Cell
cellv grid pos =
  celli grid (V.toIndex (grid#size) pos)

-- Gets the width of the grid
width :: Grid -> Int
width =
  V.x . size

-- Checks if the grid contains the pos
contains :: Grid -> Vec2 -> Bool
contains grid =
  V.contains (grid#size)

-- Transforms each cell in the grid.
--
-- @param fn A transform that receives the index and cell.
imap :: (Int -> Cell -> a) -> Grid -> [a]
imap fn grid =
  (grid#cells)
    |> Vector.ifoldr (\i cell list -> fn i cell : list) []

{- impls/setters -}
setCells :: Grid -> Vector Cell -> Grid
setCells grid cells =
  grid { cells = cells }
