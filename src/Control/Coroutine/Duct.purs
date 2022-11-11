module Control.Coroutine.Duct where

import Custom.Prelude

import Data.Bifunctor (class Bifunctor)
import Data.These (These(..))

-- | Duct captures the possibly assymetric ending of coroutines:
data Duct m n a b
  = LeftEnded a (n b)
  -- ^ Left couroutine ended with `a`, remaining right coroutine is `n b`
  | RightEnded (m a) b
  -- ^ Remaining left coroutine is `m a`, right couroutine ended with `b`, 
  | BothEnded a b -- Both coroutines ended with `a` and `b` respectively.

instance (Functor m, Functor n) ⇒ Bifunctor (Duct m n) where
  bimap ∷ ∀ a b c d. (a → b) → (c → d) → Duct m n a c → Duct m n b d
  bimap f g = case _ of
    LeftEnded a nb → LeftEnded (f a) (map g nb)
    RightEnded ma b → RightEnded (map f ma) (g b)
    BothEnded a b → BothEnded (f a) (g b)

bihoistDuct
  ∷ ∀ m n m' n' a b. (m ~> m') → (n ~> n') → Duct m n a b → Duct m' n' a b
bihoistDuct mf nf = case _ of
  LeftEnded a nb → LeftEnded a (nf nb)
  RightEnded ma b → RightEnded (mf ma) b
  BothEnded a b → BothEnded a b

toThese ∷ ∀ m n a b. Duct m n a b → These a b
toThese = case _ of
  LeftEnded a _nb → This a
  RightEnded _ma b → That b
  BothEnded a b → Both a b
