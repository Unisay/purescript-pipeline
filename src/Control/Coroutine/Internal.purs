module Control.Coroutine.Internal where

-- https://themonadreader.files.wordpress.com/2011/10/issue19.pdf

import Custom.Prelude

import Control.Apply (lift2)
import Control.Coroutine.Duct (Duct(..))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Free.Trans (FreeT, freeT)
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Parallel.Class (class Parallel, parallel, sequential)
import Data.Either.Nested (type (\/))
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))

--------------------------------------------------------------------------------

type Coroutine = FreeT

suspend ∷ ∀ f m a. Monad m ⇒ f (Coroutine f m a) → Coroutine f m a
suspend f = freeT \_ → pure (Right f)

-- | Loop until the computation returns a `Just`.
loop ∷ ∀ f m a. Functor f ⇒ Monad m ⇒ Coroutine f m (Maybe a) → Coroutine f m a
loop me = tailRecM (\_ → map (maybe (Loop unit) Done) me) unit

-- | Fuse two `Coroutine`s.
fuseWith
  ∷ ∀ f g h m a par
  . Functor f
  ⇒ Functor g
  ⇒ Functor h
  ⇒ MonadRec m
  ⇒ Parallel par m
  ⇒ (∀ b c d. (b → c → d) → f b → g c → h d)
  → Coroutine f m a
  → Coroutine g m a
  → Coroutine h m a
fuseWith zap fs gs = freeT \_ → go (Tuple fs gs)
  where
  go
    ∷ Tuple (Coroutine f m a) (Coroutine g m a)
    → m (Either a (h (Coroutine h m a)))
  go (Tuple fs' gs') = do
    next ← sequential do
      lift2 (zap Tuple)
        <$> parallel (FT.resume fs')
        <*> parallel (FT.resume gs')
    pure case next of
      Left a → Left a
      Right o → Right (map (\t → freeT \_ → go t) o)

-- | Fuse two `Coroutine`routines with a bias to the left.
fuseWithL
  ∷ ∀ f g h m a
  . Functor f
  ⇒ Functor g
  ⇒ Functor h
  ⇒ MonadRec m
  ⇒ (∀ b c d. (b → c → d) → f b → g c → h d)
  → Coroutine f m a
  → Coroutine g m a
  → Coroutine h m a
fuseWithL zap fs gs = freeT \_ → go (fs /\ gs)
  where
  go ∷ Coroutine f m a /\ Coroutine g m a → m (Either a (h (Coroutine h m a)))
  go (Tuple fs' gs') = runExceptT do
    l ← ExceptT $ FT.resume fs'
    r ← ExceptT $ FT.resume gs'
    pure $ zap Tuple l r <#> \t → freeT \_ → go t

zip
  ∷ ∀ f g h m x y
  . Functor f
  ⇒ Functor g
  ⇒ Functor h
  ⇒ MonadRec m
  ⇒ (∀ b c d. (b → c → d) → (f b → g c → h d))
  → Coroutine f m x
  → Coroutine g m y
  → Coroutine h m (Duct (Coroutine f m) (Coroutine g m) x y)
zip zap fs gs = freeT \_ → go (fs /\ gs)
  where
  go
    ∷ Coroutine f m x /\ Coroutine g m y
    → m
        ( Duct (Coroutine f m) (Coroutine g m) x y
            \/ h (Coroutine h m (Duct (Coroutine f m) (Coroutine g m) x y))
        )
  go (Tuple fs' gs') = do
    efs ← FT.resume fs'
    egs ← FT.resume gs'
    pure case efs, egs of
      Left x, Left y → Left $ BothEnded x y
      Left x, Right g → Left $ LeftEnded x (suspend g)
      Right f, Left y → Left $ RightEnded (suspend f) y
      Right f, Right g → Right $ zap Tuple f g <#> \t → freeT \_ → go t
