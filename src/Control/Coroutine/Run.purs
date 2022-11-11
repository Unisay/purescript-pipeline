module Control.Coroutine.Run where

import Custom.Prelude

import Control.Coroutine.Consumer (Consumer(..), unConsumer)
import Control.Coroutine.Duct (Duct, bihoistDuct)
import Control.Coroutine.Functor (Consume(..), consume)
import Control.Coroutine.Internal (zip)
import Control.Coroutine.Producer (Producer(..), unProducer)
import Control.Monad.Free.Trans (runFreeT)
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class (class MonadRec, Step(..), loop2, tailRecM2)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap, wrap)
import Data.Pair (Pair, pair1, pair2)

--------------------------------------------------------------------------------

runProducerConsumer
  ∷ ∀ m a x y
  . MonadRec m
  ⇒ Producer a m x
  → Consumer a m y
  → m (Duct (Producer a m) (Consumer a m) x y)
runProducerConsumer p c = bihoistDuct wrap wrap <$>
  runFreeT (pure <<< unwrap) (zip zap (unProducer p) (unConsumer c))
  where
  zap ∷ ∀ h b c d. (b → c → d) → Pair h b → Consume h c → Identity d
  zap bc pair hc = Identity (bc b (consume hc h))
    where
    h = pair1 pair
    b = pair2 pair

runProducerConsumer'
  ∷ ∀ m a x y. MonadRec m ⇒ Producer a m x → Consumer (Maybe a) m y → m (x /\ y)
runProducerConsumer' = tailRecM2 \producer consumer → do
  l ← FT.resume $ unProducer producer
  r ← FT.resume $ unConsumer consumer
  pure case l, r of
    Right p, Right (Consume f) → loop2
      (Producer $ pair2 p)
      (Consumer $ f (Just (pair1 p)))
    Right p, Left y → loop2 (Producer $ pair2 p) (pure y)
    Left x, Right (Consume f) → loop2 (pure x) (Consumer $ f Nothing)
    Left x, Left y → Done (x /\ y)
