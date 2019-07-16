module Game.Level where

-- external
import qualified System.Random as Random
import qualified Data.Tuple as Tuple
import qualified Data.Vector as Vector
import Data.Vector (Vector)

-- internal
import Core.Utils
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
init :: Random.StdGen -> Vec2 -> (Level, Random.StdGen)
init gen size =
  (seedGeneration gen size)
    |> advanceGeneration

{- impls/queries -}
-- Gets the cell at the vector position.
cell :: Level -> Vec2 -> Cell
cell level pos =
  (level#grid) Vector.! (Vec.mag pos)

{- impls/generation -}
seedGeneration :: Random.StdGen -> Vec2 -> (Level, Random.StdGen)
seedGeneration gen size =
  [0..Vec.mag size]
    |> foldr (\_ memo -> seedNextCell memo) ([], gen)
    |> mapFst (Vector.fromList)
    |> mapFst (Level size)

seedNextCell :: ([Cell], Random.StdGen) -> ([Cell], Random.StdGen)
seedNextCell (cells, gen) =
  Random.random gen
    |> mapFst (\cell -> cell : cells)

advanceGeneration :: (Level, Random.StdGen) -> (Level, Random.StdGen)
advanceGeneration level =
  level
