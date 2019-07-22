module Game where

-- internal
import Core.Utils
import qualified Core.Rand as R
import Core.Rand (Rand, RandGen)
import qualified Game.World as W
import Game.World (World)
import qualified Game.Vec as V

{- types -}
-- The game world.
data Game = Game
  { world :: W.World
  }

data Action
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | DebugStep

{- impls -}
-- Initializes the game.
--
-- @param gen A random generator
--
-- @returns The initial game state
init :: RandGen -> Rand Game
init gen =
  W.init gen
    |> R.map Game

-- Updates the game for a specific action.
--
-- @param action The action to apply to the game
-- @param game   The current game state
--
-- @returns The next game state
update :: Action -> Rand Game -> Rand Game
update action =
  R.update
    world
    (\world game -> game { world = world })
    (updateWorld action)

updateWorld :: Action -> Rand World -> Rand World
updateWorld action =
  case action of
    MoveUp ->
      W.movePlayerR (-V.uy)
    MoveDown ->
      W.movePlayerR V.uy
    MoveLeft ->
      W.movePlayerR (-V.ux)
    MoveRight ->
      W.movePlayerR V.ux
    DebugStep ->
      W.debugStepR
