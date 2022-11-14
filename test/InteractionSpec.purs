module Test.InteractionSpec where

import Custom.Prelude

import Control.Coroutine.Duct (Duct(..))
import Control.Coroutine.Interaction (Act, React, act, interact, react)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec ∷ Spec Unit
spec = describe "Interaction" do
  it "client-server ping-pong" do
    interact client server >>= case _ of
      BothEnded "Client stopped" "Server stopped" →
        pass
      BothEnded clientResult serverResult →
        fail $ "Both stopped: " <> clientResult <> ", " <> serverResult
      LeftEnded err _remainingServer →
        fail $ "Client stopped: " <> show err
      RightEnded _remainingClient err →
        fail $ "Server stopped: " <> show err

type ClientSends = Tuple String Int
type ServerReceives = ClientSends
type ServerSends = Tuple Int String
type ClientReceives = ServerSends

client ∷ Act ClientSends ClientReceives Aff String
client = 1 # tailRecM case _ of
  3 → Done "Client stopped" <$ pure "Client is stopped"
  n → Loop (n + 1) <$ act ("ping" /\ n) case _ of
    y /\ "pong" | y == n → pure $ "ping" /\ (n + 1)
    response → pure $ ("client: unexpected response: " <> show response) /\ -1

server ∷ React ServerReceives ServerSends Aff String
server = 1 # tailRecM case _ of
  3 → Done "Server stopped" <$ pure (-1 /\ "Server is stopped")
  n → Loop (n + 1) <$ react \req → pure case req of
    "ping" /\ x → x /\ "pong"
    _ /\ x → x /\ "404"
