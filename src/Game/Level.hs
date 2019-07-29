module Game.Level where

-- external
import Data.Coerce

-- internal
import qualified Core.Rand as R
import Core.Rand (Rand)
import Game.Vec (Vec2)
import qualified Game.Level.Grid as G
import Game.Level.Grid (Grid)
import qualified Game.Level.Cell as C
import Game.Level.Cell (Cell)
import qualified Game.Level.Generate as Gen

{- types -}
newtype Level
  = Level Grid

{- impls -}
-- Initializes a level.
--
-- @param size The bounding size of the level
init :: Vec2 -> Rand Level
init size =
  Level
    <$> (Gen.seed size >>= Gen.stepN 15)

{- impls/commands -}
debugStep :: Level -> Rand Level
debugStep (Level grid) =
  Level
    <$> Gen.step grid

{- impls/gridlike -}
instance G.GridLike Level where
  grid = coerce
