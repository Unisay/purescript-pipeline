module Control.Coroutine.Producer where

import Custom.Prelude

import Control.Apply (applySecond)
import Control.Coroutine.Duct (Duct(..))
import Control.Coroutine.Internal (Coroutine, loop, suspend, zip)
import Control.Monad.Except (class MonadTrans)
import Control.Monad.Free.Trans (freeT)
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class (class MonadRec, Step(..), loop2, tailRecM, tailRecM2)
import Control.Monad.Trans.Class (lift)
import Data.Align (class Align, align)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Newtype (class Newtype, over, un, unwrap, wrap)
import Data.Pair (Pair, pair, pair1, pair2)
import Data.These (These(..))
import Data.Tuple (Tuple)
import Effect.Class (class MonadEffect)

--------------------------------------------------------------------------------

newtype Producer a m x = Producer (Coroutine (Pair a) m x)

derive instance Newtype (Producer a m x) _
derive newtype instance Functor m ⇒ Functor (Producer a m)
derive newtype instance Monad m ⇒ Apply (Producer a m)
derive newtype instance Monad m ⇒ Applicative (Producer a m)
derive newtype instance Monad m ⇒ Bind (Producer a m)
derive newtype instance Monad m ⇒ MonadTrans (Producer a)
derive newtype instance MonadEffect m ⇒ MonadEffect (Producer a m)
instance Monad m ⇒ Monad (Producer a m)
derive newtype instance Monad m ⇒ MonadRec (Producer a m)

newtype ProducerF x m a = ProducerF (Coroutine (Pair a) m x)

flipPF ∷ ∀ a m r. Producer a m r → ProducerF r m a
flipPF = over Producer identity

flipFP ∷ ∀ a m r. ProducerF r m a → Producer a m r
flipFP = over ProducerF identity

overF
  ∷ ∀ b n p a m r
  . (ProducerF p n a → ProducerF r m b)
  → (Producer a n p → Producer b m r)
overF f = flipPF >>> f >>> flipFP

overP
  ∷ ∀ b n p a m r
  . (Producer a n p → Producer b m r)
  → (ProducerF p n a → ProducerF r m b)
overP f = flipFP >>> f >>> flipPF

derive instance Newtype (ProducerF r m a) _

instance Functor m ⇒ Functor (ProducerF r m) where
  map f = overP (mapP f)

mapP ∷ ∀ a b m r. Functor m ⇒ (a → b) → Producer a m r → Producer b m r
mapP f = over Producer do FT.interpret (lmap f)

instance (Semigroup r, MonadRec m) ⇒ Align (ProducerF r m) where
  align
    ∷ ∀ a b c
    . (These a b → c)
    → ProducerF r m a
    → ProducerF r m b
    → ProducerF r m c
  align f ca cb = wrap (alignF (unwrap ca) (unwrap cb))
    where
    alignF a b = zip zap a b >>= case _ of
      LeftEnded r mb → unwrap (mapP (f <<< That) (wrap mb)) <#> append r
      RightEnded ma r → unwrap (mapP (f <<< This) (wrap ma)) <#> flip append r
      BothEnded r1 r2 → pure $ r1 <> r2

    zap ∷ ∀ u h j. (u → h → j) → Pair a u → Pair b h → Pair c j
    zap t l r = pair (f (Both (pair1 l) (pair1 r))) (t (pair2 l) (pair2 r))

alignP
  ∷ ∀ a b c r m
  . Semigroup r
  ⇒ MonadRec m
  ⇒ (These a b → c)
  → Producer a m r
  → Producer b m r
  → Producer c m r
alignP f l r = flipFP (align f (flipPF l) (flipPF r))

emitP ∷ ∀ m x. Monad m ⇒ Functor (Tuple x) ⇒ x → Producer x m Unit
emitP x = Producer (suspend (pair x pass))

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
      Just (Tuple a nextState) → Right (pair a (go step nextState))

-- | Create a `Producer` that takes its values from right-folding a foldable.
producerFoldr ∷ ∀ f a m. Monad m ⇒ Foldable f ⇒ f a → Producer a m Unit
producerFoldr = foldr (applySecond <<< emitP) (pure unit)

-- | Create a `Producer` that takes its values from left-folding a foldable.
producerFoldl ∷ ∀ f a m. Monad m ⇒ Foldable f ⇒ f a → Producer a m Unit
producerFoldl = foldl (flip (applySecond <<< emitP)) (pure unit)

runProducer ∷ ∀ a m x. MonadRec m ⇒ Producer a m x → m (Array a /\ x)
runProducer = un Producer >>>
  ( identity # tailRecM2 \f g →
      FT.resume g <#> case _ of
        Right p → loop2 (f <<< Array.cons (pair1 p)) (pair2 p)
        Left x → Done $ f [] /\ x
  )
