module Test.ProducerSpec where

import Custom.Prelude

import Control.Coroutine (alignP, runProducer)
import Data.These (These(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (produceFromTo)

spec ∷ Spec Unit
spec = describe "Producer" do
  it "align" do
    outs /\ res ← runProducer $
      alignP identity (produceFromTo 1 6 $> "l") (produceFromTo 10 12 $> "r")
    outs `shouldEqual`
      [ Both 1 10
      , Both 2 11
      , Both 3 12
      , This 4
      , This 5
      , This 6
      ]
    res `shouldEqual` "lr"

