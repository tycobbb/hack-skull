module Game.Level where

-- external
import qualified System.Random as Random
import qualified Data.Tuple as Tuple
import qualified Data.Vector as Vector
import Data.Vector (Vector)

-- internal
import Utils
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
init :: Random.StdGen -> Vec2 -> Level
init gen size =
  (seedGeneration gen size)
    |> Level size
    |> advanceGeneration size

{- impls/queries -}
-- Gets the cell at the vector position.
cell :: Level -> Vec2 -> Cell
cell level pos =
  (level#grid) Vector.! (Vec.mag pos)

{- impls/generation -}
seedGeneration :: Random.StdGen -> Vec2 -> Vector Cell
seedGeneration gen size =
  [0..Vec.mag size]
    |> foldr (\_ memo -> seedNextCell memo) ([], gen)
    |> Tuple.fst
    |> Vector.fromList

seedNextCell :: ([Cell], Random.StdGen) -> ([Cell], Random.StdGen)
seedNextCell (cells, gen) =
  let
    (nextCell, nextGen) = Random.random gen
  in
    (nextCell : cells, nextGen)

advanceGeneration :: Vec2 -> Level -> Level
advanceGeneration pos level =
  level
