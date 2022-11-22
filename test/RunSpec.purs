module Test.RunSpec where

import Custom.Prelude

import Control.Coroutine (Consumer, emit, runProducerConsumer)
import Control.Coroutine.Duct (Duct(..))
import Control.Coroutine.Run (runProducerConsumers)
import Control.Monad.Rec.Class (forever)
import Data.Array ((..))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Util (consumeForever, produceFromTo, take)

spec ∷ Spec Unit
spec = describe "Run" do
  describe "runProducerConsumer" do
    it "runs till result" do
      runProducerConsumer (produceFromTo 1 5) (take 5) >>= case _ of
        LeftEnded _ _ → fail "Producer ended"
        RightEnded _ _ → fail "Consumer ended"
        BothEnded _ result → result `shouldEqual` [ 1, 2, 3, 4, 5 ]

    it "handles premature producer" do
      runProducerConsumer (produceFromTo 1 10) consumeForever >>= case _ of
        LeftEnded _ _ → pass
        RightEnded _ _ → fail "Consumer ended"
        BothEnded _ _ → fail "Both ended"

    it "handles premature consumer" do
      runProducerConsumer (forever (emit 1)) (take 5 ∷ Consumer Int _ _) >>=
        case _ of
          LeftEnded _ _ → fail "Producer ended"
          RightEnded _ _ → pass
          BothEnded _ _ → fail "Both ended"

    it "is stack safe" do
      runProducerConsumer (produceFromTo 1 100000) consumeForever >>=
        case _ of
          LeftEnded _ _ → pass -- producer is expected to end first
          RightEnded _ _ → fail "Consumer ended"
          BothEnded _ _ → fail "Both ended"

  describe "runProducerConsumers" do
    it "runs till result" do
      runProducerConsumers (produceFromTo 1 25)
        (NEA.replicate 5 (take 5)) >>= case _ of
        LeftEnded _ _ → fail "Producer ended"
        RightEnded _ _ → fail "Consumers ended"
        BothEnded _ result → result `shouldEqual`
          [ 1 .. 5, 6 .. 10, 11 .. 15, 16 .. 20, 21 .. 25 ]

    it "handles end of producer and consumer with remaining consumers" do
      runProducerConsumers (produceFromTo 1 20) (NEA.replicate 5 (take 5))
        >>= case _ of
          LeftEnded _ (consumers /\ result)
            | Array.length consumers == 1 →
                result `shouldEqual` [ 1 .. 5, 6 .. 10, 11 .. 15, 16 .. 20 ]
          LeftEnded _ (consumers /\ _) → fail
            $ "Ended with not expected amount of remaining consumers: "
            <> show (Array.length consumers)
            <> " where 1 was expected"
          RightEnded _ _ → fail "Consumers ended"
          BothEnded _ _ → fail "Both ended"

    it "handles producer being ended while consumer is still consuming" do
      runProducerConsumers (produceFromTo 1 18) (NEA.replicate 5 (take 5))
        >>= case _ of
          LeftEnded _ (consumers /\ result)
            | Array.length consumers == 2 →
                result `shouldEqual` [ 1 .. 5, 6 .. 10, 11 .. 15 ]
          LeftEnded _ (consumers /\ _) → fail
            $ "Ended with not expected amount of remaining consumers: "
            <> show (Array.length consumers)
            <> " where 2 were expected"
          RightEnded _ _ → fail "Consumers ended"
          BothEnded _ _ → fail "Both ended"

    it "handles premature consumers end" do
      runProducerConsumers (forever (emit 1)) ((NEA.replicate 5 (take 5)))
        >>= case _ of
          LeftEnded _ _ → fail "Producer ended"
          RightEnded _ result → result `shouldEqual` Array.replicate 5
            (Array.replicate 5 1)
          BothEnded _ _ → fail "Both ended"

    it "is stack free" do
      runProducerConsumers (produceFromTo 1 100000)
        (NEA.replicate 10 (take 10000))
        >>= case _ of
          LeftEnded _ _ → fail "Producer ended"
          RightEnded _ _ → fail "Consumers ended"
          BothEnded _ _ → pass
