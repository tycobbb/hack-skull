module Main where

-- system
import qualified System.IO as IO

-- internal
import Utils
import qualified Game 

{- impls -}
main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetEcho IO.stdin False
  loop Game.init
    
loop :: Game.Game -> IO ()
loop game = do
  clearScreen
  game
    |> Game.render
    |> IO.putStr
  action <- IO.getChar
    |> fmap decodeAction 
  game
    |> Game.update action
    |> loop

clearScreen :: IO ()
clearScreen =
  putStr "\ESC[H\ESC[2J"

decodeAction :: Char -> Game.Action
decodeAction char =
  case char of
    'h' -> Game.MoveLeft
    'j' -> Game.MoveDown
    'k' -> Game.MoveUp
    'l' -> Game.MoveRight
    _   -> Game.Unknown
