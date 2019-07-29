module Game.Level.Grid where

-- external
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Coerce (Coercible, coerce)

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

class GridLike a where
  grid :: a -> Grid

{- impls -}
{- impls/queryable -}
instance GridLike Grid where
  grid = id

{- impls/queries -}
-- Gets the position from a flat index
pos :: GridLike a => a -> Int -> Vec2
pos q =
  V.fromIndex (q#grid#size)

-- Gets the cell at the index
celli :: GridLike a => a -> Int -> Cell
celli q i =
  (q#grid#cells) Vector.! i

-- Gets the cell at the position
cellv :: GridLike a => a -> Vec2 -> Cell
cellv q pos =
  celli q (V.toIndex (q#grid#size) pos)

-- Gets the width of the grid
width :: GridLike a => a -> Int
width =
  V.x . size . grid

-- Checks if the grid contains the pos
contains :: GridLike a => a -> Vec2 -> Bool
contains q =
  V.contains (q#grid#size)

-- Transforms each cell in the grid.
--
-- @param fn A transform that receives the index and cell.
imap :: GridLike a => (Int -> Cell -> b) -> a -> [b]
imap fn q =
  (q#grid#cells)
    |> Vector.ifoldr (\i cell list -> fn i cell : list) []

{- impls/setters -}
setCells :: Grid -> Vector Cell -> Grid
setCells grid cells =
  grid { cells = cells }
