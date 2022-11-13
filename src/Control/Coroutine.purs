module Control.Coroutine
  ( module Producer
  , module Consumer
  , module Transducer
  , module Run
  , module Emitter
  , module Receiver
  , module Duct
  ) where

import Control.Coroutine.Consumer
  ( Consumer(..)
  , consumeWithState
  , receiveC
  , runConsumer
  ) as Consumer
import Control.Coroutine.Duct
  ( Duct(..)
  , DuctError(..)
  , SomeDuctError(..)
  , bihoistDuct
  , duct
  , leftDuct
  , mappendDuct
  , rightDuct
  , sappendDuct
  , toThese
  , unsafeLeftDuct
  , unsafeRightDuct
  ) as Duct
import Control.Coroutine.Emitter
  ( class Emitter
  , emit
  , emitM
  ) as Emitter
import Control.Coroutine.Producer
  ( Producer(..)
  , ProducerF(..)
  , alignP
  , emitP
  , emitPM
  , mapP
  , producerEffect
  , producerFoldl
  , producerFoldr
  , producerIterate
  , producerUnfold
  , runProducer
  ) as Producer
import Control.Coroutine.Receiver (class Receiver, receive) as Receiver
import Control.Coroutine.Run (runProducerConsumer, runProducerConsumer') as Run
import Control.Coroutine.Transducer
  ( Transduce(..)
  , Transducer(..)
  , appendProducerTransducer
  , composeTransducers
  , consumerTducer
  , emitT
  , emitTM
  , liftT
  , prependTransducerConsumer
  , producerTducer
  , receiveT
  , resumeT
  , scanT
  , tducerConsumer
  , tducerProcess
  , tducerProducer
  , transduceAll
  , transduceWithState
  , (<%@>)
  , (<@%>)
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
