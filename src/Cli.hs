module Cli where

-- system
import qualified System.IO as IO
import qualified System.Random as Random
import qualified System.Environment as Env
import qualified Data.Tuple as Tuple

-- internal
import Core.Utils
import qualified Core.Rand as R
import Core.Rand (Rand)
import qualified Game as G
import Game (Game)
import qualified Cli.View as V
import qualified Cli.Action as A
import qualified Cli.Config as C
import Cli.Config (Config)

{- impls -}
start :: IO ()
start = do
  IO.hSetEcho IO.stdin False
  IO.hSetBuffering IO.stdin IO.NoBuffering

  gen <-
    Random.getStdGen

  config <-
    C.init <$> Env.getArgs

  G.init
    |> loop config gen

loop :: Config -> Random.StdGen -> Rand Game -> IO ()
loop config gen rand = do
  let
    (gen', game) =
      R.call rand gen
    nextLoop =
      loop config gen'

  -- show view
  game
    |> V.render config

  -- get action from input
  action <-
    IO.getChar
      |> fmap A.decode

  -- resolve action
  case action of
    A.Game action' ->
      game
        |> G.update action'
        |> nextLoop
    A.Unknown ->
      game
        |> pure
        |> nextLoop
    A.Quit -> do
      V.reset
