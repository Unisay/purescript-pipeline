module Test.ProducerSpec where

import Custom.Prelude

import Control.Coroutine (alignP, runProducer)
import Data.Pair (pair1)
import Data.These (These(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (produceFromTo)

spec ∷ Spec Unit
spec = describe "Producer" do
  it "align" do
    result ← runProducer $
      alignP identity (produceFromTo 1 6) (produceFromTo 10 12)
    pair1 result `shouldEqual`
      [ Both 1 10
      , Both 2 11
      , Both 3 12
      , This 4
      , This 5
      , This 6
      ]

