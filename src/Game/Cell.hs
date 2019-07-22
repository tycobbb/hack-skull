module Game.Cell where

{- types -}
data Cell
  = Floor Room
  | Empty
  deriving (Show)

newtype Room =
  Room { roomId :: Int }
  deriving (Show)

{- impls -}
{- impls/queries -}
isOn :: Cell -> Bool
isOn cell =
  case cell of
    Floor _ -> True
    Empty   -> False
