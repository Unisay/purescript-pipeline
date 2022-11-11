module Control.Coroutine
  ( module Producer
  , module Consumer
  , module Transducer
  , module Run
  , module Emitter
  , module Receiver
  ) where

import Control.Coroutine.Consumer
  ( Consumer(..)
  , consumeWithState
  , receiveC
  , runConsumer
  , unConsumer
  ) as Consumer
import Control.Coroutine.Emitter (class Emitter, emit) as Emitter
import Control.Coroutine.Producer
  ( Producer(..)
  , emitP
  , producerEffect
  , producerFoldl
  , producerFoldr
  , producerIterate
  , producerUnfold
  , runProducer
  , unProducer
  ) as Producer
import Control.Coroutine.Receiver (class Receiver, receive) as Receiver
import Control.Coroutine.Run (runProducerConsumer, runProducerConsumer') as Run
import Control.Coroutine.Transducer
  ( Transduce(..)
  , Transducer(..)
  , composeTransducers
  , consumerT
  , emitT
  , liftT
  , producerT
  , receiveT
  , resumeT
  , scanT
  , transduceAll
  , transduceWithState
  , transducerC
  , transducerP
  , transducerT
  , unTransducer
  , (>->)
  ) as Transducer

--------------------------------------------------------------------------------
-- Branching -------------------------------------------------------------------

-- type Splitter a m x = Coroutine (Split a) m x

-- type Joiner a m x = Coroutine (Join a) m x

-- yieldLeft ∷ ∀ a m. Monad m ⇒ a → Splitter a m Unit
-- yieldLeft a = suspend (right (left (pair a pass)))

-- yieldRight ∷ ∀ a m. Monad m ⇒ a → Splitter a m Unit
-- yieldRight a = suspend (right (right (pair a pass)))

-- receiveLeft ∷ ∀ a m. Monad m ⇒ Joiner a m (Maybe a)
-- receiveLeft = suspend (left (left (Consume pure)))

-- receiveRight ∷ ∀ a m. Monad m ⇒ Joiner a m (Maybe a)
-- receiveRight = suspend (left (right (Consume pure)))
