module Test.Main where

import Custom.Prelude

import Control.Alternative (guard)
import Control.Coroutine
  ( Consumer
  , Producer
  , Transducer
  , await
  , awaitT
  , emit
  , producerIterate
  , runProducerConsumer
  , transduceAll
  , yieldT
  )
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Rec.Class (forever)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Coroutine Suite" do
    it "testRunProducerConsumer" do
      _ /\ result ← runProducerConsumer producer consumer
      result `shouldEqual` 53
    it "Process is stack safe" do
      let
        p = producerIterate 42 \i → pure $ guard (i < 100000) $> (i + 1)
        c = forever await
      _x /\ _y ← runProducerConsumer p c
      pass

--------------------------------------------------------------------------------
-- Producer/Consumer test ------------------------------------------------------

producer ∷ Producer Int Aff Unit
producer = do
  let fstNum = 11
  let sndNum = 42
  log $ "Producer: sending first number (" <> show fstNum <> ")..."
  emit fstNum
  log $ "Producer: sending second number (" <> show sndNum <> ")..."
  emit sndNum

consumer ∷ Consumer Int Aff Int
consumer = do
  a ← log "Consumer: waiting for the first number..." *> await
  log $ "Consumer: received first number: " <> show a
  b ← log "Consumer: waiting for the second number..." *> await
  log $ "Consumer: received second number: " <> show b
  pure $ a + b

testPrematureProducer ∷ Aff Unit
testPrematureProducer = do
  let badProducer = pass
  void $ runProducerConsumer badProducer consumer

-------- ^ Runtime Error: The producer ended too soon.

consumer2 ∷ Consumer (Maybe Int) Aff (Maybe Int)
consumer2 = runMaybeT do
  a ← log "Consumer: waiting for the first number..." *> MaybeT await
  log $ "Consumer: received first number: " <> show a
  b ← log "Consumer: waiting for the second number..." *> MaybeT await
  log $ "Consumer: received second number: " <> show b
  pure $ a + b

testRunProducerConsumer2 ∷ Aff Unit
testRunProducerConsumer2 = do
  _ /\ result ← runProducerConsumer pass consumer2
  log $ "Sum is: " <> show result

--------------------------------------------------------------------------------
-- Transducer tests ------------------------------------------------------------

double ∷ ∀ a m. Monad m ⇒ Transducer a a m Unit
double = transduceAll \a → [ a, a ]

doubleTrouble ∷ ∀ a m. Show a ⇒ MonadEffect m ⇒ Transducer a a m Unit
doubleTrouble = awaitT >>= case _ of
  Nothing → pass
  Just a → do
    log $ "Yielding first copy (" <> show a <> ") ..."
    yieldT a
    log $ "Yielding second copy (" <> show a <> ") ..."
    yieldT a

iter2 ∷ Int → Consumer (Maybe Int) Aff Unit
iter2 s = do
  n ← await <* log "Enter a number:"
  case n of
    Nothing → log $ "Sum is: " <> show s
    Just r → iter2 (s + r)

{-

> runProducer $ toProducer $ fromProducer producer >-> double
Producer: sending first number (11)...
Producer: sending second number (42)...
(Tuple [11,11,42,42] (Tuple unit unit))

> runConsumer [Just 3, Nothing] (toConsumer $ double >-> fromConsumer (iter2 0))
Enter a number:
Enter a number:
Enter a number:
Sum is: 6
(Tuple unit unit)

> run (toTrampoline $ fromProducer (emit 3) >-> double >-> fromConsumer (iter2 0))
Enter a number:
Enter a number:
Enter a number:
Sum is: 6
(Tuple unit (Tuple unit unit))

> run (toTrampoline $ fromProducer (emit 3) >-> double >-> double >-> fromConsumer (iter2 0))
Enter a number:
Enter a number:
Enter a number:
Enter a number:
Enter a number:
Sum is: 12
(Tuple unit (Tuple unit (Tuple unit unit)))

-}

