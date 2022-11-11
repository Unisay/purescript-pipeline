module Data.Pair where

import Custom.Prelude

import Data.Bifunctor (class Bifunctor, rmap)
import Data.Function.Uncurried (Fn2, runFn2)

data Pair ∷ Type → Type → Type
data Pair a b

instance Functor (Pair a) where
  map = rmap

instance Bifunctor Pair where
  bimap f g p = pair (f (pair1 p)) (g (pair2 p))

pair ∷ ∀ a b. a → b → Pair a b
pair = runFn2 mkPair

foreign import mkPair ∷ ∀ a b. Fn2 a b (Pair a b)
foreign import pair1 ∷ ∀ a b. Pair a b → a
foreign import pair2 ∷ ∀ a b. Pair a b → b
