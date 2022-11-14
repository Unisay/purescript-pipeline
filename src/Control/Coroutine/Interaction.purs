module Control.Coroutine.Interaction where

import Custom.Prelude

import Control.Coroutine.Duct (Duct, bihoistDuct)
import Control.Coroutine.Internal (Coroutine, zip)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Free.Trans (freeT, runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Class (class MonadEffect)

--------------------------------------------------------------------------------
-- Act -------------------------------------------------------------------------

newtype Action rq rp a = Action (Tuple rq (rp → a))

derive instance Functor (Action rq rp)

newtype Act rq rp m a = Act (Coroutine (Action rq rp) m a)

derive instance Newtype (Act rq rp m a) _
derive newtype instance Functor m ⇒ Functor (Act rq rp m)
derive newtype instance Monad m ⇒ Apply (Act rq rp m)
derive newtype instance Monad m ⇒ Applicative (Act rq rp m)
derive newtype instance Monad m ⇒ Bind (Act rq rp m)
derive newtype instance MonadTrans (Act rq rp)
derive newtype instance MonadThrow e m ⇒ MonadThrow e (Act rq rp m)
derive newtype instance MonadEffect m ⇒ MonadEffect (Act rq rp m)
instance Monad m ⇒ Monad (Act rq rp m)
derive newtype instance Monad m ⇒ MonadRec (Act rq rp m)

act ∷ ∀ rq rp m end. Monad m ⇒ rq → (rp → Act rq rp m end) → Act rq rp m end
act rq f = Act $ freeT \_ → pure $ Right $ Action $ Tuple rq \rp → unwrap (f rp)

--------------------------------------------------------------------------------
-- React -----------------------------------------------------------------------

newtype Reaction ∷ Type → Type → (Type → Type) → Type → Type
newtype Reaction rq rp m a = Reaction (rq → m (Tuple rp a))

derive instance Functor m ⇒ Functor (Reaction rq rp m)

newtype React ∷ Type → Type → (Type → Type) → Type → Type
newtype React rq rp m a = React (Coroutine (Reaction rq rp m) m a)

derive instance Newtype (React rq rp m a) _
derive newtype instance Functor m ⇒ Functor (React rq rp m)
derive newtype instance Monad m ⇒ Apply (React rq rp m)
derive newtype instance Monad m ⇒ Applicative (React rq rp m)
derive newtype instance Monad m ⇒ Bind (React rq rp m)
derive newtype instance MonadThrow e m ⇒ MonadThrow e (React rq rp m)
derive newtype instance MonadEffect m ⇒ MonadEffect (React rq rp m)
instance Monad m ⇒ Monad (React rq rp m)
derive newtype instance Monad m ⇒ MonadRec (React rq rp m)

react ∷ ∀ rq rp m. Monad m ⇒ (rq → m rp) → React rq rp m Unit
react f = React $ freeT \_ → pure $ Right $ Reaction \rq →
  Tuple <$> f rq <@> pass

--------------------------------------------------------------------------------
-- Interact --------------------------------------------------------------------

interact
  ∷ ∀ a b m l r
  . MonadRec m
  ⇒ Act a b m l
  → React a b m r
  → m (Duct (Act a b m) (React a b m) l r)
interact (Act cAct) (React cReact) =
  runFreeT identity $ bihoistDuct wrap wrap <$> zip f cAct cReact
  where
  f ∷ ∀ d e z. (d → e → z) → Action a b d → Reaction a b m e → m z
  f zap (Action (a /\ bd)) (Reaction abe) = do
    Tuple b e ← abe a
    pure (zap (bd b) e)
