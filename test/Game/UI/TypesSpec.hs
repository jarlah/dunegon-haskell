-- | Tests for the pure pieces of 'Game.UI.Types' — currently just
--   the CLI argument parser. These used to be trapped inside
--   @app/Main.hs@ and were therefore impossible to reach from the
--   test suite; extracting them into the library made them
--   testable, and this spec exists to make sure the extraction
--   actually pays for itself in coverage.
module Game.UI.TypesSpec (spec) where

import Test.Hspec

import Game.UI.Types (RuntimeFlags (..), parseArgs)

spec :: Spec
spec = describe "Game.UI.Types.parseArgs" $ do

  it "defaults to wizard disabled on an empty argv" $
    parseArgs [] `shouldBe` RuntimeFlags { rfWizardEnabled = False }

  it "enables wizard mode on --wizard" $
    parseArgs ["--wizard"] `shouldBe` RuntimeFlags { rfWizardEnabled = True }

  it "enables wizard mode on the -w short flag" $
    parseArgs ["-w"] `shouldBe` RuntimeFlags { rfWizardEnabled = True }

  it "enables wizard mode on --cheats" $
    parseArgs ["--cheats"] `shouldBe` RuntimeFlags { rfWizardEnabled = True }

  it "finds --wizard regardless of position" $ do
    parseArgs ["--wizard", "foo"] `shouldBe` RuntimeFlags { rfWizardEnabled = True }
    parseArgs ["foo", "--wizard"] `shouldBe` RuntimeFlags { rfWizardEnabled = True }
    parseArgs ["foo", "--wizard", "bar"]
      `shouldBe` RuntimeFlags { rfWizardEnabled = True }

  it "silently ignores unknown flags without enabling wizard mode" $
    parseArgs ["--nope", "banana"]
      `shouldBe` RuntimeFlags { rfWizardEnabled = False }

  it "does not turn on wizard mode from an unrelated substring match" $
    -- 'parseArgs' must be an exact-string match, not a substring
    -- scan. A future flag named @--wizard-style@ is not a
    -- hypothetical — this test locks in the exact-token semantics.
    parseArgs ["--wizardly"] `shouldBe` RuntimeFlags { rfWizardEnabled = False }
