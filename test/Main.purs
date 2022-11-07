module Test.Main where

import Custom.Prelude

import Control.Alternative (guard)
import Control.Coroutine
  ( Consumer
  , Fused(..)
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
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "runProducerConsumer" do
    it "runs till result" do
      runProducerConsumer producer consumer >>= case _ of
        LeftEnded _ _ → fail "Producer ended"
        RightEnded _ _ → fail "Consumer ended"
        BothEnded _ result → result `shouldEqual` 53
    it "handles premature producer" do
      let badProducer = pass
      runProducerConsumer badProducer consumer >>= case _ of
        LeftEnded _ _ → pass
        RightEnded _ _ → fail "Consumer ended"
        BothEnded _ _ → fail "Both ended"
    it "handles premature consumer" do
      runProducerConsumer producer consumer >>= case _ of
        LeftEnded _ _ → fail "Producer ended"
        RightEnded _ _ → pass
        BothEnded _ _ → fail "Both ended"
    it "is stack safe" do
      let
        p = producerIterate 42 \i → pure $ guard (i < 100000) $> (i + 1)
        c = forever await
      runProducerConsumer p c >>= case _ of
        LeftEnded _ _ → pass -- producer is expected to end first
        RightEnded _ _ → fail "Consumer ended"
        BothEnded _ _ → fail "Both ended"

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

consumer2 ∷ Consumer (Maybe Int) Aff (Maybe Int)
consumer2 = runMaybeT do
  a ← log "Consumer: waiting for the first number..." *> MaybeT await
  log $ "Consumer: received first number: " <> show a
  b ← log "Consumer: waiting for the second number..." *> MaybeT await
  log $ "Consumer: received second number: " <> show b
  pure $ a + b

--------------------------------------------------------------------------------
-- Transducer tests ------------------------------------------------------------

double ∷ ∀ a m. Monad m ⇒ Transducer a a m Unit
double = transduceAll \a → [ a, a ]

doubleTrouble ∷ ∀ a m. Show a ⇒ MonadEffect m ⇒ Transducer a a m Unit
doubleTrouble = awaitT >>= \a → do
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

