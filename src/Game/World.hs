module Game.World where

-- external
import Data.Functor
import Data.Vector (Vector)

-- internal
import Core.Extra
import qualified Core.Rand as R
import Core.Rand (Rand)
import qualified Game.Vec as V
import Game.Vec (Vec2(..))
import qualified Game.Level as L
import qualified Game.Level.Grid as LG
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
init :: Rand World
init =
  World
    <$> L.init (Vec2 30 20)
    <*> pure (Actor (Vec2 1 1))

{- impls/setters -}
setLevel :: World -> Level -> World
setLevel world level =
  world
    { level = level
    }

{- impls/commands -}
-- Moves the player by the specified offset.
--
-- @param offset The delta to move the player by
movePlayer :: Vec2 -> World -> World
movePlayer offset world =
  let
    moved = moveActor offset (world#player)
  in
    if LG.contains (world#level) (moved#pos) then
      world { player = moved }
    else
      world

moveActor :: Vec2 -> Actor -> Actor
moveActor offset actor =
  actor
    { pos = (actor#pos) + offset
    }

{- impls/commands/debug -}
debugStep :: World -> Rand World
debugStep world =
  L.debugStep (world#level)
    <&> setLevel world
