module Test.Util where

import Custom.Prelude

import Control.Alternative (guard)
import Control.Coroutine
  ( Consumer
  , Producer
  , consumeWithState
  , producerIterate
  , receive
  )
import Control.Monad.Rec.Class (class MonadRec, forever)
import Data.Array as Array
import Effect.Aff (Aff)

consumeForever ∷ ∀ a r. Consumer a Aff r
consumeForever = forever (receive ∷ Consumer a Aff a)

take ∷ ∀ a. Int → Consumer a Aff (Array a)
take n = consumeWithState Array.snoc [] \acc →
  if Array.length acc == n then Just acc else Nothing

produceFromTo ∷ ∀ m. MonadRec m ⇒ Int → Int → Producer Int m Unit
produceFromTo a b = producerIterate a \i → pure $ guard (i < b) $> (i + 1)
