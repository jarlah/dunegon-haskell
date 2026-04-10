{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Game.Logic.QuestSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Game.Logic.Quest

-- | Generator for small, realistic quest goals. We pick small
--   targets so the property loops aren't pathologically deep.
instance Arbitrary QuestGoal where
  arbitrary = oneof
    [ GoalKillMonsters <$> choose (1, 10)
    , GoalReachDepth   <$> choose (1, 10)
    , pure GoalKillBoss
    ]

instance Arbitrary QuestEvent where
  arbitrary = oneof
    [ pure EvKilledMonster
    , pure EvKilledBoss
    , EvEnteredDepth <$> choose (1, 12)
    ]

-- | Build a fresh quest for property tests.
fresh :: QuestGoal -> Quest
fresh = mkQuest "t"

spec :: Spec
spec = describe "Game.Logic.Quest" $ do

  describe "mkQuest" $ do
    it "starts at zero progress and QuestActive" $ do
      let q = mkQuest "Slayer" (GoalKillMonsters 5)
      qProgress q `shouldBe` 0
      qStatus   q `shouldBe` QuestActive

  describe "advanceQuest" $ do
    it "advances a kill quest by one per EvKilledMonster" $ do
      let q0 = fresh (GoalKillMonsters 3)
          q1 = advanceQuest EvKilledMonster q0
          q2 = advanceQuest EvKilledMonster q1
      qProgress q1 `shouldBe` 1
      qProgress q2 `shouldBe` 2
      qStatus   q2 `shouldBe` QuestActive

    it "flips a kill quest to ready-to-turn-in when the target is reached" $ do
      let q0 = fresh (GoalKillMonsters 2)
          q1 = advanceQuest EvKilledMonster q0
          q2 = advanceQuest EvKilledMonster q1
      -- Goal-met quests now wait on a turn-in instead of going
      -- straight to Completed. The GameState layer flips them to
      -- QuestCompleted when the player hands them in at an NPC.
      isReady     q2 `shouldBe` True
      isCompleted q2 `shouldBe` False

    it "ignores depth events on a kill quest" $ do
      let q0 = fresh (GoalKillMonsters 3)
          q1 = advanceQuest (EvEnteredDepth 5) q0
      q1 `shouldBe` q0

    it "tracks max depth on a depth quest (not a running sum)" $ do
      let q0 = fresh (GoalReachDepth 5)
          q1 = advanceQuest (EvEnteredDepth 2) q0
          q2 = advanceQuest (EvEnteredDepth 1) q1  -- backwards: ignored
          q3 = advanceQuest (EvEnteredDepth 4) q2
      qProgress q3 `shouldBe` 4
      qStatus   q3 `shouldBe` QuestActive

    it "flips a depth quest to ready-to-turn-in on reaching the target" $ do
      let q0 = fresh (GoalReachDepth 3)
          q1 = advanceQuest (EvEnteredDepth 3) q0
      isReady q1 `shouldBe` True

    it "overshooting a depth quest still flips to ready without going backwards" $ do
      let q0 = fresh (GoalReachDepth 3)
          q1 = advanceQuest (EvEnteredDepth 7) q0
      isReady   q1 `shouldBe` True
      qProgress q1 `shouldBe` 7

    it "flips a boss-kill quest to ready on EvKilledBoss" $ do
      let q0 = fresh GoalKillBoss
          q1 = advanceQuest EvKilledBoss q0
      qProgress q1 `shouldBe` 1
      isReady   q1 `shouldBe` True

    it "a boss-kill quest ignores regular monster kills" $ do
      let q0 = fresh GoalKillBoss
          q1 = advanceQuest EvKilledMonster q0
      q1 `shouldBe` q0

    it "a boss-kill quest ignores depth events" $ do
      let q0 = fresh GoalKillBoss
          q1 = advanceQuest (EvEnteredDepth 9) q0
      q1 `shouldBe` q0

  describe "advanceAll" $ do
    it "applies the same event to every quest in a list" $ do
      let qs  = [ fresh (GoalKillMonsters 1)
                , fresh (GoalReachDepth 1)
                ]
          qs' = advanceAll EvKilledMonster qs
      -- Only the kill quest reacts to EvKilledMonster. After one
      -- kill it's ready to turn in; the depth quest is untouched.
      map qStatus qs' `shouldBe` [QuestReadyToTurnIn, QuestActive]

  describe "QuestNotStarted (un-accepted offers)" $ do
    it "does not advance a kill quest that hasn't been accepted yet" $ do
      let offer = (fresh (GoalKillMonsters 3)) { qStatus = QuestNotStarted }
          result = advanceQuest EvKilledMonster offer
      -- Progress does not move, status stays NotStarted.
      -- (The existing advanceQuest only advances QuestActive / matching
      -- goals; this test pins down the un-accepted behavior so M10 can
      -- rely on it.)
      qProgress result `shouldBe` 0
      qStatus   result `shouldBe` QuestNotStarted

    it "does not advance a depth quest that hasn't been accepted yet" $ do
      let offer = (fresh (GoalReachDepth 3)) { qStatus = QuestNotStarted }
          result = advanceQuest (EvEnteredDepth 5) offer
      qProgress result `shouldBe` 0
      qStatus   result `shouldBe` QuestNotStarted

  describe "terminal absorption" $ do
    it "prop: a ready quest stays ready under any further event" $
      -- Goal-met but not yet turned in: the world can't push a
      -- ready quest further (no over-completing, no re-arming).
      property $ \(goal :: QuestGoal) (ev :: QuestEvent) ->
        let q0 = forceReady (fresh goal)
            q1 = advanceQuest ev q0
        in qStatus q1 == QuestReadyToTurnIn

    it "prop: a completed quest stays completed under any further event" $
      property $ \(goal :: QuestGoal) (ev :: QuestEvent) ->
        let q0 = (fresh goal) { qStatus = QuestCompleted }
            q1 = advanceQuest ev q0
        in qStatus q1 == QuestCompleted

    it "prop: a failed quest stays failed under any further event" $
      property $ \(goal :: QuestGoal) (ev :: QuestEvent) ->
        let q0 = (fresh goal) { qStatus = QuestFailed }
            q1 = advanceQuest ev q0
        in qStatus q1 == QuestFailed

    it "further kills after a ready kill quest don't bump progress" $ do
      let q0 = fresh (GoalKillMonsters 2)
          q1 = advanceQuest EvKilledMonster q0
          q2 = advanceQuest EvKilledMonster q1   -- reaches target, flips ready
          q3 = advanceQuest EvKilledMonster q2   -- post-ready, should be no-op
      qProgress q3 `shouldBe` 2
      qStatus   q3 `shouldBe` QuestReadyToTurnIn

  describe "monotonicity" $ do
    it "prop: progress never decreases after an event" $
      property $ \(goal :: QuestGoal) (ev :: QuestEvent) ->
        let q0 = fresh goal
            q1 = advanceQuest ev q0
        in qProgress q1 >= qProgress q0

    it "prop: kill-goal progress increases only under EvKilledMonster" $
      property $ \n d ->
        n > 0 ==>
        let q0 = fresh (GoalKillMonsters n)
            q1 = advanceQuest (EvEnteredDepth d) q0
        in qProgress q1 == qProgress q0

-- | Fire events until the quest flips to QuestReadyToTurnIn
--   (i.e., its goal has been met). Assumes the goal is reachable
--   (which the generator guarantees — kill / depth targets ≥ 1).
forceReady :: Quest -> Quest
forceReady q = case qGoal q of
  GoalKillMonsters n ->
    iterate (advanceQuest EvKilledMonster) q !! n
  GoalReachDepth   n ->
    advanceQuest (EvEnteredDepth n) q
  GoalKillBoss ->
    advanceQuest EvKilledBoss q
