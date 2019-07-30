module Core.Rand where

-- external
import qualified System.Random as R
import Data.Functor
import qualified Data.List as List
import qualified Data.Tuple as Tuple
import Control.Applicative

-- internal
import Core.Extra

{- types -}
newtype Rand a
  = Rand (R.StdGen -> Value a)

type Value a
  = (R.StdGen, a)

type Future a
  = R.StdGen -> Value a

type Probability a
  = (Int, a)

{- impls -}
{- impls/wrapping -}
wrap :: Future a -> Rand a
wrap =
  Rand

unwrap :: Rand a -> Future a
unwrap (Rand fn) =
  fn

{- impls/queries -}
value :: Value a -> a
value =
  Tuple.snd

{- impls/commands -}
call :: Rand a -> Future a
call =
  unwrap

{- impls/monad -}
-- i'm not sure how to write these in a nicer way
instance Functor Rand where
  fmap fn source =
    wrap (fmap fn . unwrap source)

instance Applicative Rand where
  pure value =
    wrap (\gen ->
      (gen, value))
  liftA2 fn xrand yrand =
    wrap (\gen ->
      call xrand gen
        |> (\(gen', x) ->
          call yrand gen'
            |> fmap (fn x)))

instance Monad Rand where
  (>>=) rand fn =
    wrap (\gen ->
      call rand gen
        |> (\(gen', v) ->
          call (fn v) gen'))

{- impls/factories -}
fromNative :: (R.StdGen -> (a, R.StdGen)) -> Rand a
fromNative fn =
  wrap (Tuple.swap . fn)

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
      ||> fromNative
      <&> pickValue options

sampleN :: [Probability a] -> Int -> Rand [a]
sampleN options count =
  replicate count (sample options)
    |> sequenceA
