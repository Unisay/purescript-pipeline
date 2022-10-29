module Control.Coroutine where

import Custom.Prelude

import Control.Apply (applySecond, lift2)
import Control.Coroutine.Functor (Consume(..), Join, Produce, Split, Transduce(..), produce, produce1, produce2)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Free.Trans (FreeT, freeT, runFreeT)
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class (class MonadRec, Step(..), loop2, tailRecM, tailRecM2)
import Control.Monad.Trans.Class (lift)
import Control.Parallel.Class (class Parallel, parallel, sequential)
import Data.Array as Array
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Functor.Coproduct (left, right)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
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
connect = fuseWith \f p (Consume c) → Identity (f (produce2 p) (c (produce1 p)))

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
    next ← sequential
      ( lift2 (zap Tuple)
          <$> parallel (FT.resume fs')
          <*> parallel (FT.resume gs')
      )
    case next of
      Left a → pure (Left a)
      Right o → pure (Right (map (\t → freeT \_ → go t) o))

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
fuseWithL zap fs gs = freeT \_ → go (Tuple fs gs)
  where
  go
    ∷ Tuple (Coroutine f m a) (Coroutine g m a)
    → m (Either a (h (Coroutine h m a)))
  go (Tuple fs' gs') = runExceptT do
    l ← ExceptT $ FT.resume fs'
    r ← ExceptT $ FT.resume gs'
    pure (map (\t → freeT \_ → go t) (zap Tuple l r))

--------------------------------------------------------------------------------
-- Producer --------------------------------------------------------------------

type Producer a m x = Coroutine (Produce a) m x

emit ∷ ∀ m x. Monad m ⇒ Functor (Tuple x) ⇒ x → Producer x m Unit
emit x = suspend (produce x pass)

-- | Create a `Producer` by providing a monadic function that produces values.
-- |
-- | The function should return a value of type `r` at most once, when the
-- | `Producer` is ready to close.
producerEffect ∷ ∀ o m r. Monad m ⇒ m (Either o r) → Producer o m r
producerEffect recv = loop do
  e ← lift recv
  case e of
    Left o → emit o $> Nothing
    Right r → pure (Just r)

producerIterate ∷ ∀ a m. MonadRec m ⇒ a → (a → m (Maybe a)) → Producer a m Unit
producerIterate a f = a # tailRecM \i →
  emit i *> map (maybe (Done unit) Loop) (lift (f i))

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
producerUnfold step state = freeT \_ → pure case step state of
  Nothing → Left unit
  Just (Tuple a nextState) → Right $ produce a $ producerUnfold step nextState

-- | Create a `Producer` that takes its values from right-folding a foldable.
producerFoldr ∷ ∀ f a m. Monad m ⇒ Foldable f ⇒ f a → Producer a m Unit
producerFoldr = foldr (applySecond <<< emit) (pure unit)

-- | Create a `Producer` that takes its values from left-folding a foldable.
producerFoldl ∷ ∀ f a m. Monad m ⇒ Foldable f ⇒ f a → Producer a m Unit
producerFoldl = foldl (flip (applySecond <<< emit)) (pure unit)

runProducer ∷ ∀ a m x. MonadRec m ⇒ Producer a m x → m (Produce (Array a) x)
runProducer = identity # tailRecM2 \f g →
  FT.resume g <#> case _ of
    Right p → loop2 (f <<< Array.cons (produce1 p)) (produce2 p)
    Left x → Done $ produce (f []) x

--------------------------------------------------------------------------------
-- Consumer --------------------------------------------------------------------

type Consumer a m x = Coroutine (Consume a) m x

await ∷ ∀ m x. Monad m ⇒ Functor (Tuple x) ⇒ Consumer x m x
await = suspend (Consume pure)

runConsumer ∷ ∀ a m x. MonadRec m ⇒ Array a → Consumer a m x → m x
runConsumer = tailRecM2 \is it →
  FT.resume it <#> case Array.uncons is of
    Nothing → case _ of
      Right (Consume k) → loop2 [] (k (unsafeThrow "No more inputs"))
      Left x → Done x
    Just { head, tail } → case _ of
      Right (Consume k) → loop2 tail (k head)
      Left x → Done x

runProducerConsumer
  ∷ ∀ m a x y. MonadRec m ⇒ Producer a m x → Consumer a m y → m (x /\ y)
runProducerConsumer = tailRecM2 \producer consumer → do
  l ← FT.resume producer
  r ← FT.resume consumer
  pure case l, r of
    Right p, Right (Consume f) → loop2 (produce2 p) (f (produce1 p))
    Right p, Left y → loop2 (produce2 p) (pure y)
    Left _, Right _ → unsafeThrow "The producer ended too soon."
    Left x, Left y → Done $ x /\ y

runProducerConsumer'
  ∷ ∀ m a x y. MonadRec m ⇒ Producer a m x → Consumer (Maybe a) m y → m (x /\ y)
runProducerConsumer' = tailRecM2 \producer consumer → do
  l ← FT.resume producer
  r ← FT.resume consumer
  pure case l, r of
    Right p, Right (Consume f) → loop2 (produce2 p) (f (Just (produce1 p)))
    Right p, Left y → loop2 (produce2 p) (pure y)
    Left x, Right (Consume f) → loop2 (pure x) (f Nothing)
    Left x, Left y → Done (x /\ y)

--------------------------------------------------------------------------------
-- Transducer ------------------------------------------------------------------

type Transducer a b m x = Coroutine (Transduce (Maybe a) b) m x

yieldT ∷ ∀ m a b. Monad m ⇒ b → Transducer a b m Unit
yieldT b = suspend (Supply (produce b pass))

awaitT ∷ ∀ m a b. Monad m ⇒ Transducer a b m (Maybe a)
awaitT = suspend (Demand (Consume pure))

liftT ∷ ∀ m a b. Monad m ⇒ (a → b) → Transducer a b m Unit
liftT f = awaitT >>= maybe pass \a → yieldT (f a) *> liftT f

liftStateless ∷ ∀ m a b. Monad m ⇒ (a → Array b) → Transducer a b m Unit
liftStateless f =
  awaitT >>= maybe pass \a → traverse yieldT (f a) *> liftStateless f

liftStateful
  ∷ ∀ m a b s
  . Monad m
  ⇒ (s → a → s /\ Array b)
  → (s → Array b)
  → s
  → Transducer a b m Unit
liftStateful step eof state = awaitT >>= case _ of
  Nothing → for_ (eof state) yieldT
  Just a → do
    let nextState /\ bs = step state a
    for_ bs yieldT *> liftStateful step eof nextState

composeTransducers
  ∷ ∀ a b c m x y
  . MonadRec m
  ⇒ Transducer a b m x
  → Transducer b c m y
  → Transducer a c m (x /\ y)
composeTransducers t1 t2 = freeT \_ → do
  e1 ← FT.resume t1
  e2 ← FT.resume t2
  case e1, e2 of
    Left x, Left y →
      pure $ Left $ x /\ y
    Right (Demand (Consume f)), e →
      pure $ Right $ Demand $ Consume \a → f a >-> freeT \_ → pure e
    e, Right (Supply t) →
      pure $ Right $ Supply $ composeTransducers (freeT \_ → pure e) <$> t
    Right (Supply p), Right (Demand (Consume f)) →
      FT.resume $ produce2 p >-> f (Just (produce1 p))
    Right (Supply p), Left y →
      FT.resume $ produce2 p >-> pure y
    Left x, Right (Demand (Consume f)) →
      FT.resume $ pure x >-> f Nothing

infixr 9 composeTransducers as >->

producerT ∷ ∀ a m x. MonadRec m ⇒ Producer a m x → Transducer Void a m x
producerT = FT.interpret Supply

consumerT ∷ ∀ a m x. MonadRec m ⇒ Consumer (Maybe a) m x → Transducer a Void m x
consumerT = FT.interpret Demand

transducerP ∷ ∀ a m x. MonadRec m ⇒ Transducer Void a m x → Producer a m x
transducerP = FT.interpret case _ of
  Demand _impossible → unsafeThrow "Transducer.transducerP: Demand"
  Supply t → t

transducerC
  ∷ ∀ a m x. MonadRec m ⇒ Transducer a Void m x → Consumer (Maybe a) m x
transducerC = FT.interpret case _ of
  Demand f → f
  Supply _impossible → unsafeThrow "Transducer.transducerC: Supply"

transducerT
  ∷ ∀ m x
  . MonadRec m
  ⇒ Transducer Void Void m x
  → Process m x
transducerT = FT.interpret case _ of
  Demand _impossible → unsafeThrow "Transducer.toTrampoline: Demand"
  Supply p → Identity (produce2 p)

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
