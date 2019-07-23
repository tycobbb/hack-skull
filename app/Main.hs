module Main where

-- -- internal
-- -- import qualified Cli

-- {- impls -}
-- main :: IO ()
-- main = Cli.start

import Core.Utils
import Data.List as List
import Data.Tuple as Tuple
import qualified System.Random as R
import Control.Applicative

main :: IO ()
main = do
  gen <- R.getStdGen
  putStrLn (show ((unwrap seed) gen))

data Root = Root
  { stem :: Stem
  }
  deriving (Show)

data Stem = Stem
  { leaves :: [Leaf]
  , bugs   :: [Bug]
  }
  deriving (Show)

data Leaf
  = Brown
  | Green
  deriving (Show)

data Bug
  = Ant
  | Fly
  deriving (Show)

seed :: Rand Root
seed =
  Root
    <$> seedStem

seedStem :: Rand Stem
seedStem =
  Stem
    <$> seedLeaves
    <*> seedBugs

seedLeaves :: Rand [Leaf]
seedLeaves =
  seedLeaf
    |> replicate 3
    |> sequenceA

seedLeaf :: Rand Leaf
seedLeaf =
  sample
    [ (35, Brown)
    , (65, Green)
    ]

seedBugs :: Rand [Bug]
seedBugs =
  seedBug
    |> replicate 3
    |> sequenceA

seedBug :: Rand Bug
seedBug =
  sample
    [ (35, Ant)
    , (65, Fly)
    ]

newtype Rand a = Rand
  (R.StdGen -> Value a)

type Value a
  = (a, R.StdGen)

type Future a
  = R.StdGen -> Value a

type Probability a
  = (Int, a)

{- impls/wrapping -}
wrap :: Future a -> Rand a
wrap fn =
  Rand fn

unwrap :: Rand a -> Future a
unwrap (Rand fn) =
  fn

{- impls/factories -}
sample :: [Probability a] -> Rand a
sample [] =
  error "Random.sample: empty list of options"
sample options =
  let
    findTotal =
      sum . fmap Tuple.fst
    pickValue ((_, value) : []) roll =
      value
    pickValue ((chance, value) : options) roll
      | roll < chance = value
      | otherwise     = pickValue options (roll - chance)
  in
    R.randomR (0, findTotal options)
      |> wrap
      |> fmap (pickValue options)

{- impls/applicative -}
instance Functor Rand where
  fmap fn source =
    wrap ((mapFst fn) . unwrap source)

instance Applicative Rand where
  pure value =
    wrap (\gen ->
      (value, gen))
  liftA2 fn xrand yrand =
    wrap (\gen ->
      (unwrap xrand) gen
        |> (\(x, gen') ->
          (unwrap yrand) gen'
            |> (\(y, gen'') ->
              ((fn x y), gen''))))

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst fn (l, r) =
  (fn l, r)
