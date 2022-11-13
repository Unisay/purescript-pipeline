module Control.Coroutine.Transducer where

import Custom.Prelude

import Control.Coroutine.Consumer (Consumer(..))
import Control.Coroutine.Duct (Duct(..))
import Control.Coroutine.Functor (Consume(..), consume)
import Control.Coroutine.Internal (Coroutine, suspend)
import Control.Coroutine.Process (Process)
import Control.Coroutine.Producer (Producer(..))
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (class MonadTrans)
import Control.Monad.Free.Trans (FreeT, freeT)
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Data.Identity (Identity(..))
import Data.Newtype (class Newtype, over, un, unwrap, wrap)
import Data.Pair (pair, pair1, pair2)
import Data.Traversable (for_, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import Effect.Exception.Unsafe (unsafeThrow)

--------------------------------------------------------------------------------

-- | A suspension functor that makes a coroutine which can either input
-- | or output a value every time it suspends, but not both at the same time.
-- type Transduce input output = Coproduct (Function input) (Tuple output)
data Transduce input output x
  = Demand (input → x)
  | Supply output x

derive instance Functor (Transduce a b)

--------------------------------------------------------------------------------

newtype Transducer a b m x = Transducer (Coroutine (Transduce a b) m x)

derive instance Newtype (Transducer a b m r) _
derive newtype instance Functor m ⇒ Functor (Transducer a b m)
derive newtype instance Monad m ⇒ Apply (Transducer a b m)
derive newtype instance Monad m ⇒ Applicative (Transducer a b m)
derive newtype instance Monad m ⇒ Bind (Transducer a b m)
derive newtype instance MonadTrans (Transducer a b)
derive newtype instance MonadEffect m ⇒ MonadEffect (Transducer a b m)
derive newtype instance MonadThrow e m ⇒ MonadThrow e (Transducer a b m)
instance Monad m ⇒ Monad (Transducer a b m)
derive newtype instance Monad m ⇒ MonadRec (Transducer a b m)

resumeT
  ∷ ∀ a b m r
  . MonadRec m
  ⇒ Transducer a b m r
  → m (Either r (Transduce a b (FreeT (Transduce a b) m r)))
resumeT = un Transducer >>> FT.resume

emitT ∷ ∀ m a b. Monad m ⇒ b → Transducer a b m Unit
emitT b = Transducer $ suspend $ Supply b pass

emitTM ∷ ∀ m a b. Monad m ⇒ m b → Transducer a b m Unit
emitTM = lift >=> emitT

receiveT ∷ ∀ m a b. Monad m ⇒ Transducer a b m a
receiveT = Transducer $ suspend $ Demand pure

liftT ∷ ∀ m a b. Monad m ⇒ (a → b) → Transducer a b m Void
liftT f = receiveT >>= \a → emitT (f a) *> liftT f

transduceAll ∷ ∀ m a b. Monad m ⇒ (a → Array b) → Transducer a b m Unit
transduceAll f = receiveT >>= \a → traverse emitT (f a) *> transduceAll f

scanT ∷ ∀ m i x o r. Monad m ⇒ (x → i → x) → x → (x → o) → Transducer i o m r
scanT step init done = go init
  where
  go x = do
    emitT (done x)
    i ← receiveT
    go (step x i)

transduceWithState
  ∷ ∀ m a b s
  . Monad m
  ⇒ (s → a → s /\ Array b)
  → (s → Array b)
  → s
  → Transducer a b m Unit
transduceWithState step eof state = do
  a ← receiveT
  let nextState /\ bs = step state a
  for_ bs emitT *> transduceWithState step eof nextState

composeTransducers
  ∷ ∀ a b c m x y
  . MonadRec m
  ⇒ Transducer a b m x
  → Transducer b c m y
  → Transducer a c m (Duct (Transducer a b m) (Transducer b c m) x y)
composeTransducers t1 t2 = Transducer $ freeT \_ → do
  e1 ← resumeT t1
  e2 ← resumeT t2
  let composeUnder a b = unwrap (wrap a >-> wrap b)
  case e1, e2 of
    Left x, Left y → pure $ Left $ BothEnded x y
    Left x, Right (Demand _) → pure $ Left $ LeftEnded x t2
    Right _, Left y → pure $ Left $ RightEnded t1 y
    Right (Demand f), l →
      pure $ Right $ Demand \a → f a `composeUnder` freeT \_ → pure l
    e, Right (Supply a t) →
      pure $ Right $ Supply a $ (freeT \_ → pure e) `composeUnder` t
    Right (Supply a t), Right (Demand f) → resumeT $ wrap t >-> wrap (f a)

infixr 9 composeTransducers as >->

producerTducer ∷ ∀ a m x. Functor m ⇒ Producer a m x → Transducer Void a m x
producerTducer = over Producer do FT.interpret (Supply <$> pair1 <*> pair2)

consumerTducer ∷ ∀ a m x. Functor m ⇒ Consumer a m x → Transducer a Void m x
consumerTducer = over Consumer do FT.interpret (consume >>> Demand)

tducerProducer ∷ ∀ a m x. Functor m ⇒ Transducer Void a m x → Producer a m x
tducerProducer = over Transducer do
  FT.interpret case _ of
    Demand _ → unsafeThrow "Transducer.tducerProducer: Demand"
    Supply a b → pair a b

tducerConsumer ∷ ∀ a m x. Functor m ⇒ Transducer a Void m x → Consumer a m x
tducerConsumer = over Transducer do
  FT.interpret case _ of
    Demand f → Consume f
    Supply _ _ → unsafeThrow "Transducer.tducerConsumer: Supply"

tducerProcess ∷ ∀ m x. Functor m ⇒ Transducer Void Void m x → Process m x
tducerProcess = un Transducer >>> FT.interpret case _ of
  Demand _impossible → unsafeThrow "Transducer.tducerProcess: Demand"
  Supply _ t → Identity t

appendProducerTransducer
  ∷ ∀ a b m x y
  . MonadRec m
  ⇒ Producer a m x
  → Transducer a b m y
  → Producer b m (Duct (Transducer Void a m) (Transducer a b m) x y)
appendProducerTransducer p t = tducerProducer (producerTducer p >-> t)

infixr 5 appendProducerTransducer as <@%>

prependTransducerConsumer
  ∷ ∀ a b m x y
  . MonadRec m
  ⇒ Transducer a b m x
  → Consumer b m y
  → Consumer a m (Duct (Transducer a b m) (Transducer b Void m) x y)
prependTransducerConsumer t c = tducerConsumer (t >-> consumerTducer c)

infixr 5 prependTransducerConsumer as <%@>
