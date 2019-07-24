module Cli.Config where

{- types -}
data Config = Config
  { debug :: Bool
  }

{- impls -}
init :: [String] -> Config
init args =
  Config
    { debug = elem "--debug" args
    }
