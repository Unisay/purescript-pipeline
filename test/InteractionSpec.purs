module Test.InteractionSpec where

import Custom.Prelude

import Control.Coroutine.Consumer (Consumer, receiveC)
import Control.Coroutine.Duct (Duct(..))
import Control.Coroutine.Interaction (Act, React, act, actProducerConsumer, interact, lmapAct, rcmapAct, react, reactConsumerProducer)
import Control.Coroutine.Producer (Producer, emitP)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.Writer (class MonadTell, runWriterT, tell)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec ∷ Spec Unit
spec = describe "Interaction" do

  it "client-server ping-pong" do
    runWriterT (interact client server) >>= case _ of
      BothEnded "Client stopped" "Server stopped" /\ log
        | log ==
            [ "Server received Ping (1)"
            , "Client received Pong (1)"
            , "Server received Ping (2)"
            , "Client received Pong (2)"
            , "Server received Ping (3)"
            , "Client received Pong (3)"
            , "Client stopped"
            , "Server stopped"
            ] → pass
      BothEnded clientResult serverResult /\ log →
        fail $ unwords
          [ "Both stopped:"
          , clientResult <> ","
          , serverResult <> "."
          , "Log:"
          , "\n" <> unlines log
          ]
      LeftEnded err _remainingServer /\ log →
        fail $ "Client stopped: " <> show err <> ".\nLog:\n" <> unlines log
      RightEnded _remainingClient err /\ log →
        fail $ "Server stopped: " <> show err <> ".\nLog:\n" <> unlines log

  it "lmapAct" do
    let
      f (Ping n) =
        if n == 3 then Left "Aborted"
        else Right (Ping (n * 10))
    runWriterT (interact (lmapAct f client) server) >>= case _ of
      BothEnded "Client stopped" "Server stopped" /\ log →
        fail $ "Both stopped.\nLog:\n" <> unlines log
      BothEnded clientResult serverResult /\ log →
        fail $ unwords
          [ "Both stopped:"
          , clientResult <> ","
          , serverResult <> "."
          , "Log:"
          , "\n" <> unlines log
          ]
      RightEnded _remainingClient err /\ log →
        fail $ "Server stopped: " <> show err <> ".\nLog:\n" <> unlines log
      LeftEnded err _remainingServer /\ log → do
        log `shouldEqual`
          [ "Server received Ping (10)"
          , "Client received Pong (10)"
          , "Server received Ping (20)"
          , "Client received Pong (20)"
          ]
        err `shouldEqual` "Aborted"

  it "rcmapAct" do
    let
      f (Pong n) =
        if n == 2 then Left "Aborted"
        else Right (Pong (n * 10))
    runWriterT (interact (rcmapAct f client) server) >>= case _ of
      BothEnded "Client stopped" "Server stopped" /\ log →
        fail $ "Both stopped.\nLog:\n" <> unlines log
      BothEnded clientResult serverResult /\ log →
        fail $ unwords
          [ "Both stopped:"
          , clientResult <> ","
          , serverResult <> "."
          , "Log:"
          , "\n" <> unlines log
          ]
      RightEnded _remainingClient err /\ log →
        fail $ "Server stopped: " <> show err <> ".\nLog:\n" <> unlines log
      LeftEnded err _remainingServer /\ log → do
        log `shouldEqual`
          [ "Server received Ping (1)"
          , "Client received Pong (10)"
          , "Server received Ping (2)"
          ]
        err `shouldEqual` "Aborted"

  it "actProducerConsumer" do
    let
      p ∷ ∀ m. Monad m ⇒ Producer ClientSends m String
      p = emitP (Ping 1) *> emitP (Ping 2) *> emitP (Ping 3)
        $> "No more pings, sorry"

      c ∷ ∀ m. Monad m ⇒ Consumer ClientReceives m String
      c = receiveC *> receiveC *> receiveC $> "No more pongs, please"

      renderClientResult = case _ of
        LeftEnded producerResult _remainingConsumer →
          "Client: producer ended prematurely: " <> producerResult
        RightEnded _remainingProducer consumerResult →
          "Client: consumer ended prematurely: " <> consumerResult
        BothEnded producerResult consumerResult →
          producerResult <> "; " <> consumerResult

    runWriterT (interact (actProducerConsumer p c) server) >>= case _ of
      BothEnded clientDuct serverResult /\ log → do
        renderClientResult clientDuct `shouldEqual`
          "No more pings, sorry; No more pongs, please"
        serverResult `shouldEqual` "Server stopped"
        log `shouldEqual`
          [ "Server received Ping (1)"
          , "Server received Ping (2)"
          , "Server received Ping (3)"
          , "Server stopped"
          ]
      LeftEnded clientDuct _remainingServer /\ log →
        fail $ "Client stopped: "
          <> renderClientResult clientDuct
          <> ".\nLog:\n"
          <> unlines log
      RightEnded _remainingClient err /\ log →
        fail $ "Server stopped: " <> show err <> ".\nLog:\n" <> unlines log

  it "reactConsumerProducer" do
    let
      c ∷ ∀ m. Monad m ⇒ Consumer ServerReceives m String
      c = receiveC *> receiveC *> receiveC $> "No more pings, please!"

      p ∷ ∀ m. Monad m ⇒ Producer ServerSends m String
      p = emitP (Pong 1) *> emitP (Pong 2) *> emitP (Pong 3)
        $> "Out of pongs, sorry!"

      renderServerResult = case _ of
        LeftEnded producerResult _remainingConsumer →
          "Server: producer ended prematurely: " <> producerResult
        RightEnded _remainingProducer consumerResult →
          "Server: consumer ended prematurely: " <> consumerResult
        BothEnded producerResult consumerResult →
          producerResult <> "; " <> consumerResult

    runWriterT (interact client (reactConsumerProducer c p)) >>= case _ of
      BothEnded clientResult serverDuct /\ log → do
        clientResult `shouldEqual` "Client stopped"
        renderServerResult serverDuct `shouldEqual`
          "No more pings, please!; Out of pongs, sorry!"
        log `shouldEqual`
          [ "Client received Pong (1)"
          , "Client received Pong (2)"
          , "Client received Pong (3)"
          , "Client stopped"
          ]
      LeftEnded clientResult _remainingServer /\ log →
        fail $ "Client stopped: " <> show clientResult
          <> ".\nLog:\n"
          <> unlines log
      RightEnded _remainingClient serverDuct /\ log →
        fail $ "Client stopped: "
          <> renderServerResult serverDuct
          <> ".\nLog:\n"
          <> unlines log

newtype ClientSends = Ping Int
type ServerReceives = ClientSends
newtype ServerSends = Pong Int
type ClientReceives = ServerSends
type Log = Array String

client ∷ ∀ m. MonadTell Log m ⇒ Act ClientSends ClientReceives m String
client = 1 # tailRecM case _ of
  4 → tell [ "Client stopped" ] $> Done "Client stopped"
  n → act (Ping n) \(Pong y) → do
    tell [ "Client received Pong (" <> show y <> ")" ]
    pure $ Loop (n + 1)

server ∷ ∀ m. MonadTell Log m ⇒ React ServerReceives ServerSends m String
server = 1 # tailRecM case _ of
  4 → tell [ "Server stopped" ] $> Done "Server stopped"
  n → Loop (n + 1) <$ react \(Ping x) →
    tell [ "Server received Ping (" <> show x <> ")" ]
      $> Pong x
