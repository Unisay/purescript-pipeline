module Control.Coroutine.Process where

import Custom.Prelude

import Control.Coroutine.Consumer (Consumer)
import Control.Coroutine.Functor (Consume(..))
import Control.Coroutine.Internal (Coroutine, fuseWith, suspend)
import Control.Coroutine.Producer (Producer)
import Control.Monad.Free.Trans (runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Parallel (class Parallel)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Pair (pair1, pair2)

--------------------------------------------------------------------------------

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
  fuseWith (\f p (Consume c) → Identity (f (pair2 p) (c (pair1 p))))
    (unwrap producer)
    (unwrap consumer)
