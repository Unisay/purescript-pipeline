module Test.Main where

import Custom.Prelude

import Control.Coroutine (Consumer, Producer, Transducer, emit, receive, receiveT, transduceAll)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Test.ConsumerSpec as Consumer
import Test.ProducerSpec as Producer
import Test.RunSpec as Run
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.TransducerSpec as Transducer

main ∷ Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  Run.spec
  Producer.spec
  Consumer.spec
  Transducer.spec

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
  a ← log "Consumer: waiting for the first number..." *> receive
  log $ "Consumer: received first number: " <> show a
  b ← log "Consumer: waiting for the second number..." *> receive
  log $ "Consumer: received second number: " <> show b
  pure $ a + b

consumer2 ∷ Consumer (Maybe Int) Aff (Maybe Int)
consumer2 = runMaybeT do
  a ← log "Consumer: waiting for the first number..." *> MaybeT receive
  log $ "Consumer: received first number: " <> show a
  b ← log "Consumer: waiting for the second number..." *> MaybeT receive
  log $ "Consumer: received second number: " <> show b
  pure $ a + b

--------------------------------------------------------------------------------
-- Transducer tests ------------------------------------------------------------

double ∷ ∀ a m. Monad m ⇒ Transducer a a m Unit
double = transduceAll \a → [ a, a ]

doubleTrouble ∷ ∀ a m. Show a ⇒ MonadEffect m ⇒ Transducer a a m Unit
doubleTrouble = receiveT >>= \a → do
  log $ "Yielding first copy (" <> show a <> ") ..."
  emit a
  log $ "Yielding second copy (" <> show a <> ") ..."
  emit a

iter2 ∷ Int → Consumer (Maybe Int) Aff Unit
iter2 s = do
  n ← receive <* log "Enter a number:"
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
