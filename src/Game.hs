module Game where

-- external
import qualified System.Random as Random

-- internal
import Core.Utils
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

{- impls -}
-- Initializes the game.
--
-- @param gen A random generator
--
-- @returns The initial game state
init :: Random.StdGen -> (Game, Random.StdGen)
init gen =
  W.init gen
    |> mapFst initGame

initGame :: World -> Game
initGame world =
  Game
  { world = world
  }

-- Updates the game for a specific action.
--
-- @param action The action to apply to the game
-- @param game   The current game state
--
-- @returns The next game state
update :: Action -> (Game, Random.StdGen) -> (Game, Random.StdGen)
update action game =
  case action of
    MoveUp ->
      game
        |> updateWorld (W.movePlayer (-V.uy))
    MoveDown ->
      game
        |> updateWorld (W.movePlayer V.uy)
    MoveLeft ->
      game
        |> updateWorld (W.movePlayer (-V.ux))
    MoveRight ->
      game
        |> updateWorld (W.movePlayer V.ux)

updateWorld :: (W.World -> W.World) -> (Game, Random.StdGen) -> (Game, Random.StdGen)
updateWorld fn (game, gen) =
  (game {
    world = fn (game#world)
  }, gen)
