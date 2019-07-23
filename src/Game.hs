module Game where

-- internal
import Core.Utils
import qualified Core.Rand as R
import Core.Rand (Rand)
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
init :: Rand Game
init =
  Game
    <$> W.init

{- impls/setters -}
setWorld :: Game -> World -> Game
setWorld game world =
  game
    { world = world
    }

{- impls/commands -}
-- Updates the game for a specific action.
--
-- @param action The action to apply to the game
-- @param game   The current game state
--
-- @returns The next game state
update :: Action -> Game -> Rand Game
update action game =
  (game#world)
    |> updateWorld action
    |> fmap (setWorld game)
  where
    updateWorld action =
      case action of
        MoveUp ->
          pure . W.movePlayer (-V.uy)
        MoveDown ->
          pure . W.movePlayer V.uy
        MoveLeft ->
          pure . W.movePlayer (-V.ux)
        MoveRight ->
          pure . W.movePlayer V.ux
        DebugStep ->
          W.debugStep
