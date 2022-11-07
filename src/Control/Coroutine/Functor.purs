module Control.Coroutine.Functor where

import Custom.Prelude

import Data.Bifunctor (class Bifunctor, rmap)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Functor.Coproduct (Coproduct)

newtype Consume a b = Consume (a → b)

consume ∷ ∀ a b. Consume a b → a → b
consume (Consume f) = f

derive newtype instance Functor (Consume a)

data Produce ∷ Type → Type → Type
data Produce a b

instance Functor (Produce a) where
  map = rmap

instance Bifunctor Produce where
  bimap f g p = produce (f (produce1 p)) (g (produce2 p))

produce ∷ ∀ a b. a → b → Produce a b
produce = runFn2 mkProduce

foreign import mkProduce ∷ ∀ a b. Fn2 a b (Produce a b)
foreign import produce1 ∷ ∀ a b. Produce a b → a
foreign import produce2 ∷ ∀ a b. Produce a b → b

-- | A suspension functor that makes a coroutine which supplies an `output`
-- | and consumes an `input` before it can proceed.
type CoTransform output input a = Produce output (Consume input a)

-- | A suspension functor that makes a coroutine which consumes an `input` 
-- | and proceeds by supplying an `output`.
type Transform output input a = Consume input (Produce output a)

type Split a = Coproduct
  (Consume (Maybe a))
  (Coproduct (Produce a) (Produce a))

type Join a = Coproduct
  (Coproduct (Consume (Maybe a)) (Consume (Maybe a)))
  (Produce a)

