module Game.World where

-- external
import Data.Vector (Vector)

-- internal
import Core.Utils
import qualified Core.Rand as R
import Core.Rand (Rand, RandGen)
import qualified Game.Vec as V
import Game.Vec (Vec2)
import qualified Game.Level as L
import Game.Level (Level)

{- types -}
-- The game world.
data World = World
  { level  :: Level
  , player :: Actor
  }

-- A movable actor
data Actor = Actor
  { pos :: Vec2
  }

{- impls -}
-- Constructs a world with a valid initial state.
--
-- @param gen A random generator
--
-- @return The initial world state
init :: RandGen -> Rand World
init gen =
  L.init gen (V.Vec2 30 20)
    |> R.map initWorld

initWorld :: Level -> World
initWorld level =
  World
  { level = level
  , player = Actor (V.Vec2 1 1)
  }

{- impls/commands -}
-- Moves the player by the specified offset.
--
-- @param offset The delta to move the player by
movePlayerR :: Vec2 -> Rand World -> Rand World
movePlayerR offset =
  R.map (movePlayer offset)

movePlayer :: Vec2 -> World -> World
movePlayer offset world =
  let
    moved = moveActor offset (world#player)
  in
    if V.contains (moved#pos) (world#level#L.size) then
      world { player = moved }
    else
      world

moveActor :: Vec2 -> Actor -> Actor
moveActor offset actor =
  actor
  { pos = (actor#pos) + offset
  }

{- impls/commands/debug -}
debugStepR :: Rand World -> Rand World
debugStepR =
  R.update
    level
    (\level world -> world { level = level })
    L.stepR

{- impls/queries -}
width :: World -> Int
width =
  V.x . L.size . level
