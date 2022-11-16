module Test.Cards where

import Custom.Prelude

import Control.Coroutine (Consumer, Producer)
import Control.Coroutine.Consumer (consumeWithState)
import Control.Coroutine.Duct (Duct(..))
import Control.Coroutine.Producer (producerUnfold)
import Control.Coroutine.Run (runProducerConsumer)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array ((..))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Enum (toEnum)
import Data.NonEmpty ((:|))
import Data.Profunctor.Strong ((&&&))
import Data.String (CodePoint)
import Data.String.CodePoints (fromCodePointArray)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Test.QuickCheck (randomSeed)
import Test.QuickCheck.Gen (evalGen)
import Test.QuickCheck.Gen as Gen

type Generator = Producer Card Aff Unit

type Player = Consumer Card Aff (Array Card)

type Card = CodePoint

type Deck = Array Card

main ∷ Effect Unit
main = launchAff_ do
  newSeed ← liftEffect randomSeed
  let shuffledDeck = evalGen (Gen.shuffle deck) { newSeed, size: 36 }
  foldCards shuffledDeck (createPlayers 7)

deck ∷ Deck
deck = Array.catMaybes $ toEnum <$> firstCardCodepoint .. lastCardCodepoint
  where
  firstCardCodepoint = 0x1F0A0
  lastCardCodepoint = 0x1F0C4

generator ∷ Deck → Generator
generator = producerUnfold do
  NEA.fromArray <<#>> NEA.head &&& (NEA.toArray >>> Array.drop 1)

player ∷ Player
player = consumeWithState Array.snoc [] $
  guarded \arr → Array.length arr >= handCapacity
  where
  handCapacity = 6

createPlayers ∷ Int → Array Player
createPlayers = flip Array.replicate player

foldCards ∷ Deck → Array Player → Aff Unit
foldCards = generator >>> go 1
  where
  go counter gen players = case Array.uncons players of
    Nothing → log "All players were provided with cards"
    Just { head: nextPlayer, tail: remainingPlayers } →
      runProducerConsumer gen nextPlayer >>= case _ of
        BothEnded _ consumerRes → do
          log
            $ "Player "
            <> show counter
            <> ": "
            <> fromCodePointArray consumerRes
          log "The deck-bottom was reached"
        LeftEnded _ _ → log
          "The deck capacity is too low for such an amount of players"
        RightEnded gen' consumerRes → do
          log
            $ "Player "
            <> show counter
            <> ": "
            <> fromCodePointArray consumerRes
          go (counter + 1) gen' remainingPlayers
