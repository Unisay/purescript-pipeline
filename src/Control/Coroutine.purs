module Control.Coroutine where

import Custom.Prelude

import Control.Apply (applySecond, lift2)
import Control.Coroutine.Duct (Duct(..), bihoistDuct)
import Control.Coroutine.Functor
  ( Consume(..)
  , Join
  , Produce
  , Split
  , consume
  , produce
  , produce1
  , produce2
  )
import Control.Monad.Except (class MonadTrans, ExceptT(..), runExceptT)
import Control.Monad.Free.Trans (FreeT, freeT, runFreeT)
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class
  ( class MonadRec
  , Step(..)
  , loop2
  , tailRecM
  , tailRecM2
  )
import Control.Monad.Trans.Class (lift)
import Control.Parallel.Class (class Parallel, parallel, sequential)
import Data.Array as Array
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Functor.Coproduct (left, right)
import Data.Identity (Identity(..))
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import Effect.Exception.Unsafe (unsafeThrow)

-- https://themonadreader.files.wordpress.com/2011/10/issue19.pdf

type Coroutine = FreeT

suspend ∷ ∀ f m a. Monad m ⇒ f (Coroutine f m a) → Coroutine f m a
suspend f = freeT \_ → pure (Right f)

-- | Loop until the computation returns a `Just`.
loop ∷ ∀ f m a. Functor f ⇒ Monad m ⇒ Coroutine f m (Maybe a) → Coroutine f m a
loop me = tailRecM (\_ → map (maybe (Loop unit) Done) me) unit

--------------------------------------------------------------------------------
-- Process ---------------------------------------------------------------------

type Process m x = Coroutine Identity m x

pause ∷ ∀ m. Monad m ⇒ Process m Unit
pause = suspend (pure pass)

-- | Run a `Process` to completion.
runProcess ∷ ∀ m a. MonadRec m ⇒ Process m a → m a
runProcess = runFreeT (pure <<< unwrap)

-- | Connect a producer and a consumer.
connect
  ∷ ∀ o f m a
  . MonadRec m
  ⇒ Parallel f m
  ⇒ Producer o m a
  → Consumer o m a
  → Process m a
connect producer consumer =
  fuseWith (\f p (Consume c) → Identity (f (produce2 p) (c (produce1 p))))
    (unwrap producer)
    (unwrap consumer)

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

--------------------------------------------------------------------------------
-- Producer --------------------------------------------------------------------

newtype Producer a m x = Producer (Coroutine (Produce a) m x)

derive instance Newtype (Producer a m x) _
derive newtype instance Functor m ⇒ Functor (Producer a m)
derive newtype instance Monad m ⇒ Apply (Producer a m)
derive newtype instance Monad m ⇒ Applicative (Producer a m)
derive newtype instance Monad m ⇒ Bind (Producer a m)
derive newtype instance Monad m ⇒ MonadTrans (Producer a)
derive newtype instance MonadEffect m ⇒ MonadEffect (Producer a m)
instance Monad m ⇒ Monad (Producer a m)
derive newtype instance Monad m ⇒ MonadRec (Producer a m)

unProducer ∷ ∀ a m x. Producer a m x → Coroutine (Produce a) m x
unProducer = unwrap

emitP ∷ ∀ m x. Monad m ⇒ Functor (Tuple x) ⇒ x → Producer x m Unit
emitP x = Producer (suspend (produce x pass))

-- | Create a `Producer` by providing a monadic function that produces values.
-- |
-- | The function should return a value of type `r` at most once, when the
-- | `Producer` is ready to close.
producerEffect ∷ ∀ a m r. Monad m ⇒ m (Either a r) → Producer a m r
producerEffect recv = Producer $ loop do
  lift recv >>= case _ of
    Left o → unwrap (emitP o $> Nothing)
    Right r → pure (Just r)

producerIterate ∷ ∀ a m. MonadRec m ⇒ a → (a → m (Maybe a)) → Producer a m Unit
producerIterate a f = Producer $ a # tailRecM \i →
  unwrap (emitP i) *> map (maybe (Done unit) Loop) (lift (f i))

-- | Create a `Producer` by providing a pure function that generates next output
-- |
-- | This could have been an instance of the `Unfoldable` type class:
-- | `unfoldr :: forall a b. (b -> Maybe (Tuple a b)) -> b -> t a`
producerUnfold
  ∷ ∀ a s m
  . Applicative m
  ⇒ (s → Maybe (Tuple a s))
  → s
  → Producer a m Unit
producerUnfold step' = Producer <<< go step'
  where
  go step state = freeT \_ →
    pure case step state of
      Nothing → Left unit
      Just (Tuple a nextState) → Right (produce a (go step nextState))

-- | Create a `Producer` that takes its values from right-folding a foldable.
producerFoldr ∷ ∀ f a m. Monad m ⇒ Foldable f ⇒ f a → Producer a m Unit
producerFoldr = foldr (applySecond <<< emit) (pure unit)

-- | Create a `Producer` that takes its values from left-folding a foldable.
producerFoldl ∷ ∀ f a m. Monad m ⇒ Foldable f ⇒ f a → Producer a m Unit
producerFoldl = foldl (flip (applySecond <<< emit)) (pure unit)

runProducer ∷ ∀ a m x. MonadRec m ⇒ Producer a m x → m (Produce (Array a) x)
runProducer = unProducer >>>
  ( identity # tailRecM2 \f g →
      FT.resume g <#> case _ of
        Right p → loop2 (f <<< Array.cons (produce1 p)) (produce2 p)
        Left x → Done $ produce (f []) x
  )

--------------------------------------------------------------------------------
-- Consumer --------------------------------------------------------------------

newtype Consumer a m x = Consumer (Coroutine (Consume a) m x)

derive instance Newtype (Consumer a m x) _
derive newtype instance Functor m ⇒ Functor (Consumer a m)
derive newtype instance Monad m ⇒ Apply (Consumer a m)
derive newtype instance Monad m ⇒ Applicative (Consumer a m)
derive newtype instance Monad m ⇒ Bind (Consumer a m)
derive newtype instance Monad m ⇒ MonadTrans (Consumer a)
derive newtype instance MonadEffect m ⇒ MonadEffect (Consumer a m)
instance Monad m ⇒ Monad (Consumer a m)
derive newtype instance Monad m ⇒ MonadRec (Consumer a m)

unConsumer ∷ ∀ a m x. Consumer a m x → Coroutine (Consume a) m x
unConsumer = unwrap

awaitC ∷ ∀ m x. Monad m ⇒ Consumer x m x
awaitC = Consumer (suspend (Consume pure))

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
    a ← awaitC
    let s' = step s a
    maybe (go s') pure (last s')

runConsumer ∷ ∀ a m x. MonadRec m ⇒ Array a → Consumer a m x → m x
runConsumer = tailRecM2 \is it →
  FT.resume (unConsumer it) <#> case Array.uncons is of
    Nothing → case _ of
      Right (Consume k) → loop2 [] (Consumer $ k (unsafeThrow "No more inputs"))
      Left x → Done x
    Just { head, tail } → case _ of
      Right (Consume k) → loop2 tail (Consumer $ k head)
      Left x → Done x

runProducerConsumer
  ∷ ∀ m a x y
  . MonadRec m
  ⇒ Producer a m x
  → Consumer a m y
  → m (Duct (Producer a m) (Consumer a m) x y)
runProducerConsumer p c = bihoistDuct wrap wrap <$>
  runFreeT (pure <<< unwrap) (zip zap (unProducer p) (unConsumer c))
  where
  zap ∷ ∀ h b c d. (b → c → d) → Produce h b → Consume h c → Identity d
  zap bc produce hc = Identity (bc b (consume hc h))
    where
    h = produce1 produce
    b = produce2 produce

runProducerConsumer'
  ∷ ∀ m a x y. MonadRec m ⇒ Producer a m x → Consumer (Maybe a) m y → m (x /\ y)
runProducerConsumer' = tailRecM2 \producer consumer → do
  l ← FT.resume $ unProducer producer
  r ← FT.resume $ unConsumer consumer
  pure case l, r of
    Right p, Right (Consume f) → loop2
      (Producer $ produce2 p)
      (Consumer $ f (Just (produce1 p)))
    Right p, Left y → loop2 (Producer $ produce2 p) (pure y)
    Left x, Right (Consume f) → loop2 (pure x) (Consumer $ f Nothing)
    Left x, Left y → Done (x /\ y)

--------------------------------------------------------------------------------
-- Transducer ------------------------------------------------------------------

-- | A suspension functor that makes a coroutine which can either input
-- | or output a value every time it suspends, but not both at the same time.
-- type Transduce input output = Coproduct (Function input) (Tuple output)
data Transduce input output x
  = Demand (input → x)
  | Supply output x

derive instance Functor (Transduce a b)

newtype Transducer a b m x = Transducer (Coroutine (Transduce a b) m x)

derive instance Newtype (Transducer a b m r) _
derive newtype instance Functor m ⇒ Functor (Transducer a b m)
derive newtype instance Monad m ⇒ Apply (Transducer a b m)
derive newtype instance Monad m ⇒ Applicative (Transducer a b m)
derive newtype instance Monad m ⇒ Bind (Transducer a b m)
derive newtype instance Monad m ⇒ MonadTrans (Transducer a b)
derive newtype instance MonadEffect m ⇒ MonadEffect (Transducer a b m)
instance Monad m ⇒ Monad (Transducer a b m)
derive newtype instance Monad m ⇒ MonadRec (Transducer a b m)

unTransducer ∷ ∀ a b m r. Transducer a b m r → Coroutine (Transduce a b) m r
unTransducer = unwrap

resumeT
  ∷ ∀ a b m r
  . MonadRec m
  ⇒ Transducer a b m r
  → m (Either r (Transduce a b (FreeT (Transduce a b) m r)))
resumeT = unTransducer >>> FT.resume

emitT ∷ ∀ m a b. Monad m ⇒ b → Transducer a b m Unit
emitT b = Transducer $ suspend $ Supply b pass

awaitT ∷ ∀ m a b. Monad m ⇒ Transducer a b m a
awaitT = Transducer $ suspend $ Demand pure

liftT ∷ ∀ m a b. Monad m ⇒ (a → b) → Transducer a b m Unit
liftT f = awaitT >>= \a → emitT (f a) *> liftT f

transduceAll ∷ ∀ m a b. Monad m ⇒ (a → Array b) → Transducer a b m Unit
transduceAll f = awaitT >>= \a → traverse emitT (f a) *> transduceAll f

scanT ∷ ∀ m i x o r. Monad m ⇒ (x → i → x) → x → (x → o) → Transducer i o m r
scanT step init done = go init
  where
  go x = do
    emitT (done x)
    i ← awaitT
    go (step x i)

transduceWithState
  ∷ ∀ m a b s
  . Monad m
  ⇒ (s → a → s /\ Array b)
  → (s → Array b)
  → s
  → Transducer a b m Unit
transduceWithState step eof state = do
  a ← awaitT
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
    Left x, Right _ → pure $ Left $ LeftEnded x t2
    Right _, Left y → pure $ Left $ RightEnded t1 y
    Right (Demand f), l →
      pure $ Right $ Demand \a → f a `composeUnder` freeT \_ → pure l
    e, Right (Supply a t) →
      pure $ Right $ Supply a $ (freeT \_ → pure e) `composeUnder` t
    Right (Supply a t), Right (Demand f) → resumeT $ wrap t >-> wrap (f a)

infixr 9 composeTransducers as >->

producerT ∷ ∀ a m x. MonadRec m ⇒ Producer a m x → Transducer Void a m x
producerT = over Producer do FT.interpret (Supply <$> produce1 <*> produce2)

consumerT ∷ ∀ a m x. MonadRec m ⇒ Consumer a m x → Transducer a Void m x
consumerT = over Consumer do FT.interpret (consume >>> Demand)

transducerP ∷ ∀ a m x. MonadRec m ⇒ Transducer Void a m x → Producer a m x
transducerP = over Transducer do
  FT.interpret case _ of
    Demand _ → unsafeThrow "Transducer.transducerP: Demand"
    Supply a b → produce a b

transducerC ∷ ∀ a m x. MonadRec m ⇒ Transducer a Void m x → Consumer a m x
transducerC = over Transducer do
  FT.interpret case _ of
    Demand f → Consume f
    Supply _ _ → unsafeThrow "Transducer.transducerC: Supply"

transducerT ∷ ∀ m x. MonadRec m ⇒ Transducer Void Void m x → Process m x
transducerT = unTransducer >>> FT.interpret case _ of
  Demand _impossible → unsafeThrow "Transducer.toTrampoline: Demand"
  Supply _ t → Identity t

--------------------------------------------------------------------------------
-- Common behaviors ------------------------------------------------------------

class Emitter c a where
  emit ∷ a → c Unit

instance Monad m ⇒ Emitter (Producer a m) a where
  emit = emitP

instance Monad m ⇒ Emitter (Transducer a b m) b where
  emit = emitT

class Receiver ∷ (Type → Type) → Type → Constraint
class Receiver c a where
  await ∷ c a

instance Monad m ⇒ Receiver (Consumer a m) a where
  await = awaitC

instance Monad m ⇒ Receiver (Transducer a b m) a where
  await = awaitT

--------------------------------------------------------------------------------
-- Branching -------------------------------------------------------------------

type Splitter a m x = Coroutine (Split a) m x

type Joiner a m x = Coroutine (Join a) m x

yieldLeft ∷ ∀ a m. Monad m ⇒ a → Splitter a m Unit
yieldLeft a = suspend (right (left (produce a pass)))

yieldRight ∷ ∀ a m. Monad m ⇒ a → Splitter a m Unit
yieldRight a = suspend (right (right (produce a pass)))

awaitLeft ∷ ∀ a m. Monad m ⇒ Joiner a m (Maybe a)
awaitLeft = suspend (left (left (Consume pure)))

awaitRight ∷ ∀ a m. Monad m ⇒ Joiner a m (Maybe a)
awaitRight = suspend (left (right (Consume pure)))
