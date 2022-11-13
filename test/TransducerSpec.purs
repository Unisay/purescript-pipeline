module Test.TransducerSpec where

import Custom.Prelude

import Control.Coroutine.Consumer (runConsumer)
import Control.Coroutine.Duct (Duct(..), absurdLeft)
import Control.Coroutine.Producer (emitP, runProducer)
import Control.Coroutine.Run (runProducerConsumer)
import Control.Coroutine.Transducer (Transducer, liftT, scanT, (<%@>), (<@%>))
import Control.Monad.Rec.Class (forever)
import Data.Array ((..))
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Util (produceFromTo, take)

spec ∷ Spec Unit
spec = describe "Transducer" do
  it "scans" do
    let
      t ∷ ∀ r. Transducer Int String Aff r
      t = scanT (\s a → s <> show a) "" identity
    runProducerConsumer (forever (emitP 3)) (t <%@> take 5) >>= case _ of
      LeftEnded _ _ → fail "Producer ended"
      BothEnded _ _ → fail "Both ended"
      RightEnded _ res → case res of
        BothEnded _ _ → fail "Both transduced"
        LeftEnded _unit _ → fail "Transduced first"
        RightEnded _ r → r `shouldEqual` [ "", "3", "33", "333", "3333" ]
  it "appends to producer" do
    out /\ _ ← runProducer (produceFromTo 1 5 <@%> liftT show)
    out `shouldEqual` [ "1", "2", "3", "4", "5" ]
  it "prepends to consumer" do
    d ← runConsumer (1 .. 5) (liftT show <%@> take 5)
    case absurdLeft d of
      Left _t → fail "Remaining consumer"
      Right res → res `shouldEqual` [ "1", "2", "3", "4", "5" ]

