module Game.Cell where

{- types -}
data Cell
  = Floor Room
  | Empty
  deriving (Show)

newtype Room =
  Room Int
  deriving (Show, Ord, Eq)

{- impls -}
{- impls/commands -}
thenA :: Applicative f => f Cell -> Cell -> f Cell
thenA continuation cell =
  case cell of
    Empty -> continuation
    _     -> pure cell

{- impls/queries -}
isEmpty :: Cell -> Bool
isEmpty cell =
  case cell of
    Empty -> True
    _     -> False

room :: Cell -> Maybe Room
room cell =
  case cell of
    Floor room -> Just room
    Empty      -> Nothing
