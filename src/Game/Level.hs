module Game.Level where

-- external
import qualified Data.Tuple as Tuple
import qualified Data.Vector as Vector
import Data.Vector (Vector)

-- internal
import Core.Utils
import qualified Core.Rand as R
import Core.Rand (Rand, RandGen)
import qualified Game.Vec as Vec
import Game.Vec (Vec2)

{- types -}
type Cell  = Bool
data Level = Level
  { size :: Vec2
  , grid :: Vector Cell
  }

{- impls -}
-- Initializes a level.
--
-- @param gen  A random generator
-- @param size The bounding size of the level
--
-- @return A new level
init :: RandGen -> Vec2 -> Rand Level
init gen size =
  (seedGeneration gen size)
    |> advanceGeneration

{- impls/queries -}
-- Gets the cell at the vector position.
cell :: Level -> Vec2 -> Cell
cell level pos =
  (level#grid) Vector.! (Vec.mag pos)

{- impls/generation -}
seedGeneration :: RandGen -> Vec2 -> Rand Level
seedGeneration gen size =
  R.generate (Vec.mag size) gen
    |> R.map Vector.fromList
    |> R.map (Level size)

advanceGeneration :: Rand Level -> Rand Level
advanceGeneration level =
  level
