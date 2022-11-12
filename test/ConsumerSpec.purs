module Test.ConsumerSpec where

import Prelude

import Control.Coroutine (emit, runProducerConsumer)
import Control.Coroutine.Duct (Duct(..))
import Control.Monad.Rec.Class (forever)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Util (take)

spec ∷ Spec Unit
spec = describe "Consumer" do
  it "consumes input with state" do
    runProducerConsumer (forever (emit 3)) (take 4) >>= case _ of
      LeftEnded _ _ → fail "Producer ended"
      BothEnded _ _ → fail "Both ended"
      RightEnded _ res → res `shouldEqual` [ 3, 3, 3, 3 ]
