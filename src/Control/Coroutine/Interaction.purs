module Control.Coroutine.Interaction where

import Custom.Prelude

import Control.Bind (bindFlipped)
import Control.Coroutine.Consumer (Consume, Consumer)
import Control.Coroutine.Duct (Duct, bihoistDuct)
import Control.Coroutine.Internal (Coroutine, zip)
import Control.Coroutine.Producer (Producer)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Free.Trans (FreeT, freeT, runFreeT)
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Bifunctor (lmap)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Pair (Pair, pair1, pair2)
import Effect.Class (class MonadEffect)

--------------------------------------------------------------------------------
-- Act -------------------------------------------------------------------------

-- | A suspension functor that makes a coroutine which supplies an `output`
-- | and consumes an `input` before it can proceed.
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

actProducerConsumer
  ∷ ∀ a b m p c
  . MonadRec m
  ⇒ Producer a m p
  → Consumer b m c
  → Act a b m (Duct (Producer a m) (Consumer b m) p c)
actProducerConsumer p c =
  wrap $ bihoistDuct wrap wrap <$> zip zap (unwrap p) (unwrap c)
  where
  zap ∷ ∀ x y z. (x → y → z) → Pair a x → Consume b y → Action a b z
  zap xyz ax by = Action (pair1 ax) \b → xyz (pair2 ax) (unwrap by b)

codimapAct
  ∷ ∀ a b c d m r
  . Functor m
  ⇒ (a → b)
  → (c → d)
  → Act a d m r
  → Act b c m r
codimapAct ab cd = over Act do
  FT.interpret \(Action a dk) → Action (ab a) (cd >>> dk)

lmapAct
  ∷ ∀ a b c m r
  . MonadRec m
  ⇒ (a → Either r b)
  → Act a c m r
  → Act b c m r
lmapAct f = over Act lmapFAct
  where
  lmapFAct ∷ FreeT (Action a c) m r → FreeT (Action b c) m r
  lmapFAct co = freeT \_ →
    FT.resume co <#> bindFlipped \(Action a k) →
      f a <#> \b → Action b (lmapFAct <$> k)

rcmapAct
  ∷ ∀ a b c m r
  . MonadRec m
  ⇒ (c → Either r b)
  → Act a b m r
  → Act a c m r
rcmapAct f = over Act rcmapFAct
  where
  rcmapFAct ∷ FreeT (Action a b) m r → FreeT (Action a c) m r
  rcmapFAct co = freeT \_ →
    FT.resume co <<#>> \(Action a k) →
      Action a $ either pure (rcmapFAct <<< k) <<< f

--------------------------------------------------------------------------------
-- React -----------------------------------------------------------------------

-- | A suspension functor that makes a coroutine which consumes a `q`
-- | and proceeds by running effect producing its result `r`.
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
instance MonadTrans (React q r) where
  lift = wrap <<< lift

react ∷ ∀ q r m. Monad m ⇒ (q → m r) → React q r m Unit
react f = React $ freeT \_ → pure $ Right $ wrap \q → Tuple <$> f q <@> pass

reactConsumerProducer
  ∷ ∀ a b m p c
  . MonadRec m
  ⇒ Consumer a m c
  → Producer b m p
  → React a b m (Duct (Consumer a m) (Producer b m) c p)
reactConsumerProducer c p =
  wrap $ bihoistDuct wrap wrap <$> zip zap (unwrap c) (unwrap p)
  where
  zap ∷ ∀ x y z. (x → y → z) → Consume a x → Pair b y → Reaction a b m z
  zap xyz ax by = wrap \a → pure (pair1 by /\ xyz (unwrap ax a) (pair2 by))

dimapReact
  ∷ ∀ a b c d m r
  . Functor m
  ⇒ (a → b)
  → (c → d)
  → React b c m r
  → React a d m r
dimapReact ab cd = over React do
  FT.interpret \r → wrap (ab >>> unwrap r >>> map (lmap cd))

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
