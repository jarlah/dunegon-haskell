{-# LANGUAGE ScopedTypeVariables #-}
module Game.Logic.ProgressionSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Game.Logic.Progression
import Game.Types (Stats(..))

-- | Reasonable "living player" stat block: level and XP are in sane
--   ranges, HP positive and ≤ maxHP. We keep levels small so quadratic
--   thresholds don't blow out test time.
genPlayerStats :: Gen Stats
genPlayerStats = do
  lvl   <- choose (1, 20)
  mhp   <- choose (1, 200)
  hp    <- choose (1, mhp)
  atk   <- choose (0, 30)
  dfn   <- choose (0, 30)
  spd   <- choose (1, 20)
  xp    <- choose (0, xpForNextLevel lvl - 1)
  pure Stats
    { sHP = hp, sMaxHP = mhp
    , sAttack = atk, sDefense = dfn, sSpeed = spd
    , sLevel = lvl, sXP = xp
    }

spec :: Spec
spec = describe "Game.Logic.Progression" $ do

  it "prop_xpNeverNegative" $ property $
    forAll genPlayerStats $ \s ->
    forAll (choose (0 :: Int, 10000)) $ \xp ->
      let (s', _) = gainXP s xp
      in sXP s' >= 0

  it "prop_gainZeroXPIsIdentity" $ property $
    forAll genPlayerStats $ \s ->
      gainXP s 0 === (s, 0)

  it "prop_gainXPNeverLosesLevel" $ property $
    forAll genPlayerStats $ \s ->
    forAll (choose (0 :: Int, 10000)) $ \xp ->
      let (s', _) = gainXP s xp
      in sLevel s' >= sLevel s

  it "prop_levelUpFullHeals" $ property $
    forAll genPlayerStats $ \s ->
      let s' = levelUp s
      in sHP s' === sMaxHP s'

  it "prop_levelUpIncreasesMaxHP" $ property $
    forAll genPlayerStats $ \s ->
      sMaxHP (levelUp s) > sMaxHP s

  it "prop_levelUpBumpsLevelByOne" $ property $
    forAll genPlayerStats $ \s ->
      sLevel (levelUp s) === sLevel s + 1

  it "prop_xpCurveStrictlyMonotonic" $ property $
    \(Positive (n :: Int)) ->
      xpForNextLevel n < xpForNextLevel (n + 1)

  it "prop_multiLevelUpConsumesCorrectXP" $ property $
    forAll genPlayerStats $ \s ->
    forAll (choose (0 :: Int, 5000)) $ \xp ->
      let (s', ups) = gainXP s xp
      -- Number of level-ups matches the level delta.
      in sLevel s' - sLevel s === ups
         -- And leftover XP is always strictly under the new threshold.
         .&&. sXP s' < xpForNextLevel (sLevel s')

  it "prop_xpRewardPositive" $ property $
    conjoin
      [ counterexample (show k) (xpReward k > 0)
      | k <- [minBound .. maxBound]
      ]
