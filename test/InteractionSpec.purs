module Test.InteractionSpec where

import Custom.Prelude

import Control.Coroutine.Duct (Duct(..))
import Control.Coroutine.Interaction (Act, React, act, interact, react)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.Writer (class MonadTell, runWriterT, tell)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

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
