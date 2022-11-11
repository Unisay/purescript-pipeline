module Control.Coroutine.Emitter where

import Custom.Prelude

import Control.Coroutine.Producer (Producer, emitP)
import Control.Coroutine.Transducer (Transducer, emitT)

class Emitter c a where
  emit ∷ a → c Unit

instance Monad m ⇒ Emitter (Producer a m) a where
  emit = emitP

instance Monad m ⇒ Emitter (Transducer a b m) b where
  emit = emitT
