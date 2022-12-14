module Control.Coroutine.Consumer where

import Custom.Prelude

import Control.Coroutine.Internal (Coroutine, suspend)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (class MonadTrans)
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class (class MonadRec, Step(..), loop2, tailRecM2)
import Data.Array as Array
import Data.Newtype (class Newtype, over, un)
import Data.Profunctor (class Profunctor, lcmap)
import Effect.Class (class MonadEffect)
import Effect.Exception.Unsafe (unsafeThrow)

--------------------------------------------------------------------------------

newtype Consume a b = Consume (Function a b)

derive instance Newtype (Consume a b) _
derive newtype instance Profunctor Consume
derive newtype instance Functor (Consume a)

--------------------------------------------------------------------------------

newtype Consumer a m x = Consumer (Coroutine (Consume a) m x)

derive instance Newtype (Consumer a m x) _
derive newtype instance Functor m ⇒ Functor (Consumer a m)
derive newtype instance Monad m ⇒ Apply (Consumer a m)
derive newtype instance Monad m ⇒ Applicative (Consumer a m)
derive newtype instance Monad m ⇒ Bind (Consumer a m)
derive newtype instance MonadTrans (Consumer a)
derive newtype instance MonadThrow e m ⇒ MonadThrow e (Consumer a m)
derive newtype instance MonadEffect m ⇒ MonadEffect (Consumer a m)
instance Monad m ⇒ Monad (Consumer a m)
derive newtype instance Monad m ⇒ MonadRec (Consumer a m)

receiveC ∷ ∀ m x. Monad m ⇒ Consumer x m x
receiveC = Consumer (suspend (Consume pure))

lcmapC ∷ ∀ a b m r. Functor m ⇒ (b → a) → Consumer a m r → Consumer b m r
lcmapC f = over Consumer do FT.interpret (lcmap f)

hoistC ∷ ∀ a m n r. Functor n ⇒ (m ~> n) → Consumer a m r → Consumer a n r
hoistC f = over Consumer do FT.hoistFreeT f

consumeWithState
  ∷ ∀ a m s x
  . Monad m
  ⇒ (s → a → s)
  -- ^ step function that produces next state
  → s
  -- ^ initial state
  → (s → Maybe x)
  -- ^ Nothing: continue consuming input, Just x: terminate consumer with x
  → Consumer a m x
consumeWithState step init last = go init
  where
  go s = do
    a ← receiveC
    let s' = step s a
    maybe (go s') pure (last s')

runConsumer ∷ ∀ a m x. MonadRec m ⇒ Array a → Consumer a m x → m x
runConsumer = tailRecM2 \is it →
  FT.resume (un Consumer it) <#> case Array.uncons is of
    Nothing → case _ of
      Right (Consume k) → loop2 [] (Consumer $ k (unsafeThrow "No more inputs"))
      Left x → Done x
    Just { head, tail } → case _ of
      Right (Consume k) → loop2 tail (Consumer $ k head)
      Left x → Done x
