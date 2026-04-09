-- | Tests for 'Game.Logic.Chest'. Pure unit tests over the loot
--   roll, the per-turn tick, and the bulk re-entry refill walk.
module Game.Logic.ChestSpec (spec) where

import System.Random (mkStdGen)
import Test.Hspec

import Game.Logic.Chest
import Game.Types
import Linear (V2 (..))

dummyPos :: Pos
dummyPos = V2 0 0

spec :: Spec
spec = do

  describe "rollChestLoot" $ do

    it "produces an item drawn from the chest loot table for any seed" $ do
      let allowed = map snd chestLootTable
      mapM_
        (\s ->
           let (item, _) = rollChestLoot (mkStdGen s)
           in (item `elem` allowed) `shouldBe` True)
        [0 .. 200 :: Int]

    it "is deterministic for a fixed seed" $ do
      let (a, _) = rollChestLoot (mkStdGen 42)
          (b, _) = rollChestLoot (mkStdGen 42)
      a `shouldBe` b

    it "covers every table entry over a large sample" $ do
      -- Structural: each item in the loot table should appear
      -- at least once across 400 seeded rolls. Catches a broken
      -- weighted picker that locks onto a single entry.
      let rolls = [ fst (rollChestLoot (mkStdGen s)) | s <- [0 .. 400] ]
      mapM_
        (\item -> (item `elem` rolls) `shouldBe` True)
        (map snd chestLootTable)

  describe "tickChest" $ do

    it "is a no-op on a full chest" $ do
      let c  = Chest { chestPos = dummyPos
                     , chestState = ChestFull (IPotion HealingMinor) }
      tickChest c `shouldBe` c

    it "decrements an empty chest's cooldown by one" $ do
      let c  = Chest { chestPos = dummyPos, chestState = ChestEmpty 7 }
      chestState (tickChest c) `shouldBe` ChestEmpty 6

    it "clamps a zero cooldown at zero (never goes negative)" $ do
      let c  = Chest { chestPos = dummyPos, chestState = ChestEmpty 0 }
      chestState (tickChest c) `shouldBe` ChestEmpty 0

    it "does not move an empty chest's position" $ do
      let p  = V2 5 9
          c  = Chest { chestPos = p, chestState = ChestEmpty 4 }
      chestPos (tickChest c) `shouldBe` p

  describe "refillChests" $ do

    it "refills a chest whose cooldown has expired" $ do
      let c0       = Chest { chestPos = dummyPos, chestState = ChestEmpty 0 }
          (cs', _) = refillChests (mkStdGen 1) [c0]
      case cs' of
        [Chest { chestState = ChestFull _ }] -> pure ()
        other -> expectationFailure
                   ("expected a ChestFull, got: " ++ show other)

    it "leaves still-counting-down chests alone" $ do
      let c0       = Chest { chestPos = dummyPos, chestState = ChestEmpty 5 }
          (cs', _) = refillChests (mkStdGen 1) [c0]
      cs' `shouldBe` [c0]

    it "leaves already-full chests alone" $ do
      let c0       = Chest { chestPos = dummyPos
                           , chestState = ChestFull (IPotion HealingMinor) }
          (cs', _) = refillChests (mkStdGen 1) [c0]
      cs' `shouldBe` [c0]

    it "walks a mixed list and refills only the expired entries" $ do
      let c1 = Chest { chestPos = V2 1 1, chestState = ChestEmpty 0 }
          c2 = Chest { chestPos = V2 2 2, chestState = ChestEmpty 3 }
          c3 = Chest { chestPos = V2 3 3
                     , chestState = ChestFull (IPotion HealingMajor) }
          c4 = Chest { chestPos = V2 4 4, chestState = ChestEmpty 0 }
          (cs', _) = refillChests (mkStdGen 7) [c1, c2, c3, c4]
      length cs' `shouldBe` 4
      case cs' of
        [r1, r2, r3, r4] -> do
          case chestState r1 of
            ChestFull _ -> pure ()
            s -> expectationFailure ("c1 expected full, got " ++ show s)
          chestPos r1 `shouldBe` chestPos c1
          r2 `shouldBe` c2
          r3 `shouldBe` c3
          case chestState r4 of
            ChestFull _ -> pure ()
            s -> expectationFailure ("c4 expected full, got " ++ show s)
          chestPos r4 `shouldBe` chestPos c4
        _ -> expectationFailure "unreachable"

  describe "chestRespawnTurns" $ do
    it "is a positive cooldown (sanity check)" $
      (chestRespawnTurns > 0) `shouldBe` True
