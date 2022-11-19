module Control.Coroutine.Duct where

import Custom.Prelude

import Data.Bifunctor (class Bifunctor)
import Data.These (These(..))
import Partial.Unsafe (unsafeCrashWith)

-- | Duct captures the possibly assymetric ending of coroutines:
data Duct m n a b
  = LeftEnded a (n b)
  -- ^ Left couroutine ended with `a`, remaining right coroutine is `n b`
  | RightEnded (m a) b
  -- ^ Remaining left coroutine is `m a`, right couroutine ended with `b`,
  | BothEnded a b -- Both coroutines ended with `a` and `b` respectively.

duct
  ∷ ∀ m n a b r
  . (a → n b → r)
  → (m a → b → r)
  → (a → b → r)
  → Duct m n a b
  → r
duct left right both = case _ of
  LeftEnded a nb → left a nb
  RightEnded ma b → right ma b
  BothEnded a b → both a b

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
toThese = duct (const <<< This) (const That) Both

data DuctError a b = ErrLeftEnded a | ErrRightEnded b

instance (Show a, Show b) ⇒ Show (DuctError a b) where
  show = case _ of
    ErrLeftEnded a → "ErrLeftEnded " <> show a
    ErrRightEnded b → "ErrRightEnded " <> show b

appendDuctE ∷ ∀ m n s. Semigroup s ⇒ Duct m n s s → Either (DuctError s s) s
appendDuctE = case _ of
  LeftEnded a _nb → Left (ErrLeftEnded a)
  RightEnded _ma b → Left (ErrRightEnded b)
  BothEnded s1 s2 → Right (s1 <> s2)

appendDuct ∷ ∀ m n s. Semigroup s ⇒ Duct m n s s → s
appendDuct = case _ of
  LeftEnded s _nb → s
  RightEnded _ma s → s
  BothEnded s1 s2 → s1 <> s2

leftDuct ∷ ∀ m n a. Duct m n a Unit → Either (DuctError a Unit) a
leftDuct = case _ of
  LeftEnded a _nb → Right a
  RightEnded _ma b → Left (ErrRightEnded b)
  BothEnded a _b → Right a

unsafeLeftDuct ∷ ∀ m n a b. Duct m n a b → a
unsafeLeftDuct = case _ of
  LeftEnded a _nb → a
  RightEnded _ma _unit → unsafeCrashWith "unsafeLeftDuct: right ended"
  BothEnded a _unit → a

rightDuct ∷ ∀ m n b. Duct m n Unit b → Either (DuctError Unit b) b
rightDuct = case _ of
  LeftEnded a _nb → Left (ErrLeftEnded a)
  RightEnded _ma b → Right b
  BothEnded _a b → Right b

unsafeRightDuct ∷ ∀ m n a b. Duct m n a b → b
unsafeRightDuct = case _ of
  LeftEnded _a _nb → unsafeCrashWith "unsafeRightDuct: left ended"
  RightEnded _ma b → b
  BothEnded _a b → b

absurdDuct ∷ ∀ m n a. Duct m n Void Void → a
absurdDuct = case _ of
  LeftEnded void _nb → absurd void
  RightEnded _ma void → absurd void
  BothEnded void _b → absurd void

absurdLeft ∷ ∀ m n b. Duct m n Void b → Either (n b) b
absurdLeft = case _ of
  LeftEnded a _nb → absurd a
  RightEnded _mv b → Right b
  BothEnded a _b → absurd a

absurdRight ∷ ∀ m n a. Duct m n a Void → Either (m a) a
absurdRight = case _ of
  LeftEnded a _nv → Right a
  RightEnded _ma b → absurd b
  BothEnded _a b → absurd b
