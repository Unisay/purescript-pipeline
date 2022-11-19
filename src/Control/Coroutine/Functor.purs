module Control.Coroutine.Functor where

import Custom.Prelude

import Data.Functor.Coproduct (Coproduct)
import Data.Newtype (class Newtype)
import Data.Pair (Pair)
import Data.Profunctor (class Profunctor)

newtype Consume a b = Consume (a → b)

derive instance Newtype (Consume a b) _
derive newtype instance Profunctor Consume
derive newtype instance Functor (Consume a)

consume ∷ ∀ a b. Consume a b → a → b
consume (Consume f) = f

-- | A suspension functor that makes a coroutine which supplies an `output`
-- | and consumes an `input` before it can proceed.
type CoTransform output input a = Pair output (Consume input a)

-- | A suspension functor that makes a coroutine which consumes an `input`
-- | and proceeds by supplying an `output`.
type Transform output input a = Consume input (Pair output a)

type Split a = Coproduct
  (Consume (Maybe a))
  (Coproduct (Pair a) (Pair a))

type Join a = Coproduct
  (Coproduct (Consume (Maybe a)) (Consume (Maybe a)))
  (Pair a)

