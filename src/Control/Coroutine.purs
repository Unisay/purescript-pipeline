module Control.Coroutine
  ( module Producer
  , module Consumer
  , module Transducer
  , module Run
  , module Emitter
  , module Receiver
  , module Duct
  , module Interaction
  , exceptResult
  , tryExceptCo
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
  , absurdDuct
  , absurdLeft
  , absurdRight
  , appendDuct
  , appendDuctE
  , bihoistDuct
  , duct
  , leftDuct
  , rightDuct
  , toThese
  , unsafeLeftDuct
  , unsafeRightDuct
  ) as Duct
import Control.Coroutine.Emitter (class Emitter, emit, emitM) as Emitter
import Control.Coroutine.Interaction
  ( Act(..)
  , Action(..)
  , React(..)
  , Reaction(..)
  , act
  , actProducerConsumer
  , codimapAct
  , dimapReact
  , interact
  , lmapAct
  , rcmapAct
  , react
  , reactConsumerProducer
  ) as Interaction
import Control.Coroutine.Internal (Coroutine)
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
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Free.Trans (FreeT, freeT)
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class (class MonadRec)
import Custom.Prelude (class Functor, Either(..), either, map, (<#>), (>>>))
import Data.Bifunctor (bimap)
import Data.Newtype (class Newtype, over, wrap)

exceptResult
  ∷ ∀ m e r f t s
  . Functor f
  ⇒ MonadRec m
  ⇒ Newtype t (FreeT f (ExceptT e m) r)
  ⇒ Newtype s (FreeT f m (Either e r))
  ⇒ t
  → s
exceptResult = over wrap tryExceptCo

tryExceptCo
  ∷ ∀ m e r f
  . Functor f
  ⇒ MonadRec m
  ⇒ Coroutine f (ExceptT e m) r
  → Coroutine f m (Either e r)
tryExceptCo co = freeT \_ →
  runExceptT (FT.resume co) <#>
    either (Left >>> Left) (bimap Right (map tryExceptCo))

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
