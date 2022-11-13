module Control.Coroutine.Emitter where

import Custom.Prelude

import Control.Coroutine.Producer (Producer, emitP, emitPM)
import Control.Coroutine.Transducer (Transducer, emitT, emitTM)

class Emitter c m a | c m → a where
  emit ∷ a → c m Unit
  emitM ∷ m a → c m Unit

instance Monad m ⇒ Emitter (Producer a) m a where
  emit = emitP
  emitM = emitPM

instance Monad m ⇒ Emitter (Transducer a b) m b where
  emit = emitT
  emitM = emitTM
