-- | Tests for the slash-command parser. The parser is tiny today
--   (only @/reveal@), but the tests establish the conventions for
--   future commands: whitespace tolerance, optional leading @/@,
--   case-insensitivity, and a useful error for unknown verbs.
module Game.Logic.CommandSpec (spec) where

import Test.Hspec

import Game.Logic.Command

spec :: Spec
spec = describe "Game.Logic.Command.parseCommand" $ do

  it "parses 'reveal'" $
    parseCommand "reveal" `shouldBe` Right CmdReveal

  it "parses '/reveal' with a leading slash" $
    parseCommand "/reveal" `shouldBe` Right CmdReveal

  it "tolerates leading whitespace" $
    parseCommand "   reveal" `shouldBe` Right CmdReveal

  it "is case-insensitive on the verb" $ do
    parseCommand "REVEAL" `shouldBe` Right CmdReveal
    parseCommand "Reveal" `shouldBe` Right CmdReveal

  it "rejects the empty buffer" $
    parseCommand "" `shouldBe` Left "empty command"

  it "rejects an unknown command with a helpful message" $
    parseCommand "teleport" `shouldBe` Left "unknown command: teleport"
