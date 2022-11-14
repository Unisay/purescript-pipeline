module Control.Coroutine.Interaction where

import Custom.Prelude

import Control.Coroutine.Duct (Duct, bihoistDuct)
import Control.Coroutine.Internal (Coroutine, zip)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Free.Trans (freeT, runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Bifunctor (lmap)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Class (class MonadEffect)

--------------------------------------------------------------------------------
-- Act -------------------------------------------------------------------------

data Action q r a = Action q (r → a)

derive instance Functor (Action q r)

newtype Act q r m a = Act (Coroutine (Action q r) m a)

derive instance Newtype (Act q r m a) _
derive newtype instance Functor m ⇒ Functor (Act q r m)
derive newtype instance Monad m ⇒ Apply (Act q r m)
derive newtype instance Monad m ⇒ Applicative (Act q r m)
derive newtype instance Monad m ⇒ Bind (Act q r m)
derive newtype instance MonadTrans (Act q r)
derive newtype instance MonadThrow e m ⇒ MonadThrow e (Act q r m)
derive newtype instance MonadEffect m ⇒ MonadEffect (Act q r m)
derive newtype instance MonadTell w m ⇒ MonadTell w (Act q r m)
instance Monad m ⇒ Monad (Act q r m)
derive newtype instance Monad m ⇒ MonadRec (Act q r m)

act ∷ ∀ q r m end. Monad m ⇒ q → (r → Act q r m end) → Act q r m end
act q f = Act $ freeT \_ → pure $ Right $ Action q \r → unwrap (f r)

--------------------------------------------------------------------------------
-- React -----------------------------------------------------------------------

newtype Reaction ∷ Type → Type → (Type → Type) → Type → Type
newtype Reaction q r m a = Reaction (q → m (Tuple r a))

derive instance Newtype (Reaction q r m a) _
derive instance Functor m ⇒ Functor (Reaction q r m)

newtype React ∷ Type → Type → (Type → Type) → Type → Type
newtype React q r m a = React (Coroutine (Reaction q r m) m a)

derive instance Newtype (React q r m a) _
derive newtype instance Functor m ⇒ Functor (React q r m)
derive newtype instance Monad m ⇒ Apply (React q r m)
derive newtype instance Monad m ⇒ Applicative (React q r m)
derive newtype instance Monad m ⇒ Bind (React q r m)
derive newtype instance MonadThrow e m ⇒ MonadThrow e (React q r m)
derive newtype instance MonadEffect m ⇒ MonadEffect (React q r m)
derive newtype instance MonadTell w m ⇒ MonadTell w (React q r m)
instance Monad m ⇒ Monad (React q r m)
derive newtype instance Monad m ⇒ MonadRec (React q r m)

react ∷ ∀ q r m. Monad m ⇒ (q → m r) → React q r m Unit
react f = React $ freeT \_ → pure $ Right $ wrap \q → Tuple <$> f q <@> pass

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
  f ∷ ∀ k k1 k2. (k1 → k2 → k) → Action a b k1 → Reaction a b m k2 → m k
  f zap (Action q rpk1) (Reaction r) = uncurry zap <<< lmap rpk1 <$> r q
