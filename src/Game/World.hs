module Game.World where

-- internal
import Utils
import qualified Game.Vec as V

{- types -}
-- The game world.
data World = World
  { size :: V.Vec2
  , player :: Actor
  }

-- A movable actor
data Actor = Actor
  { pos :: V.Vec2
  }

{- impls -}
-- Constructs a world with a valid initial state.
init :: World
init =
  World
  { size   = V.Vec2 30 20
  , player = Actor (V.Vec2 1 1)
  }

{- impls/commands -}
-- Moves the player by the specified offset.
--
-- @param offset The delta to move the player by
movePlayer :: V.Vec2 -> World -> World
movePlayer offset world =
  let
    moved = moveActor offset (world#player)
  in
    if V.contains (moved#pos) (world#size) then
      world { player = moved }
    else
      world

moveActor :: V.Vec2 -> Actor -> Actor
moveActor offset actor =
  actor
  { pos = (actor#pos) + offset
  }
