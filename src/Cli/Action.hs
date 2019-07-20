module Cli.Action where

import qualified Game

{- types -}
data Action
  = Game Game.Action
  | Quit
  | Unknown

{- impls -}
{- impls/queries -}
decode :: Char -> Action
decode char =
  case char of
    'h' -> Game Game.MoveLeft
    'j' -> Game Game.MoveDown
    'k' -> Game Game.MoveUp
    'l' -> Game Game.MoveRight
    'd' -> Game Game.DebugStep
    'q' -> Quit
    _   -> Unknown
