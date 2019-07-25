module Game.Level where

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

{- impls/queries -}
pos :: Level -> Int -> Vec2
pos (Level grid) =
  G.pos grid

width :: Level -> Int
width (Level grid) =
  G.width grid

contains :: Level -> Vec2 -> Bool
contains (Level grid) =
  G.contains grid

imap :: (Int -> Cell -> a) -> Level -> [a]
imap fn (Level grid) =
  G.imap fn grid
