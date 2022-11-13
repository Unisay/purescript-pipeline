module Control.Coroutine.Receiver where

import Custom.Prelude

import Control.Coroutine.Consumer (Consumer, receiveC)
import Control.Coroutine.Transducer (Transducer, receiveT)

class Receiver ∷ (Type → Type) → Type → Constraint
class Receiver c a | c → a where
  receive ∷ c a

instance Monad m ⇒ Receiver (Consumer a m) a where
  receive = receiveC

instance Monad m ⇒ Receiver (Transducer a b m) a where
  receive = receiveT
