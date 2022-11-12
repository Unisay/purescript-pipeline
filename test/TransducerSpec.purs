module Test.TransducerSpec where

import Custom.Prelude

import Control.Coroutine
  ( Transducer
  , consumerT
  , emit
  , runProducerConsumer
  , scanT
  , transducerC
  , (>->)
  )
import Control.Coroutine.Duct (Duct(..))
import Control.Monad.Rec.Class (forever)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Util (take)

spec ∷ Spec Unit
spec = describe "Transducer" do
  it "scans" do
    let
      t ∷ ∀ r. Transducer Int String Aff r
      t = scanT (\s a → s <> show a) "" identity
      c = transducerC (t >-> consumerT (take 5))
    runProducerConsumer (forever (emit 3)) c >>= case _ of
      LeftEnded _ _ → fail "Producer ended"
      BothEnded _ _ → fail "Both ended"
      RightEnded _ res → case res of
        BothEnded _ _ → fail "Both transduced"
        LeftEnded _unit _ → fail "Transduced first"
        RightEnded _ outputs →
          outputs `shouldEqual` [ "", "3", "33", "333", "3333" ]
