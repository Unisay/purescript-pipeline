module Test.RunSpec where

import Custom.Prelude

import Control.Coroutine (Consumer, emit, runProducerConsumer)
import Control.Coroutine.Duct (Duct(..))
import Control.Monad.Rec.Class (forever)
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
