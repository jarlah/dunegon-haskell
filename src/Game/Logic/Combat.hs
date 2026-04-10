module Game.Logic.Combat
  ( Damage(..)
  , CombatResult(..)
  , HitOutcome(..)
  , resolveWith
  , resolveAttack
  , applyDamage
  , isDead
  , resultDamage
  , describeAttack
  , describeAttacked
  , applyHitResult
  , monsterCombatEvent
  ) where

import System.Random (StdGen, randomR)

import Game.Types
  ( Pos, Stats(..), Monster(..), Item, GameEvent(..), isBoss, monsterName, itemName
  )
import Game.Logic.Quest (Quest, QuestEvent(..), fireQuestEvent)
import qualified Game.Logic.Progression as P
import qualified Game.Logic.Loot as Loot
import Game.Utils.List (updateAt, removeAt)

newtype Damage = Damage { unDamage :: Int }
  deriving (Eq, Ord, Show)

data CombatResult
  = Miss
  | Hit         Damage
  | CriticalHit Damage
  | Kill        Damage
  deriving (Eq, Show)

-- | Pure attack resolution given two pre-rolled dice.
--
--   @roll@ is a d20 for to-hit; @critRoll@ is a d100 where <=10 crits.
--   All of combat's actual rules live here so they can be tested
--   deterministically without threading 'StdGen'.
resolveWith :: Int -> Int -> Stats -> Stats -> CombatResult
resolveWith roll critRoll atk def
  | isDead atk          = Miss
  | roll < hitThreshold = Miss
  | otherwise           =
      let base    = max 1 (sAttack atk - sDefense def `div` 2)
          critted = critRoll <= 10
          d       = Damage (if critted then base * 2 else base)
      in if sHP def - unDamage d <= 0
           then Kill        d
           else if critted
                  then CriticalHit d
                  else Hit         d
  where
    hitThreshold = max 5 (10 + sDefense def - sAttack atk)

-- | RNG-threading wrapper around 'resolveWith'.
resolveAttack :: StdGen -> Stats -> Stats -> (CombatResult, StdGen)
resolveAttack gen0 atk def =
  let (roll,     gen1) = randomR (1 :: Int, 20)  gen0
      (critRoll, gen2) = randomR (1 :: Int, 100) gen1
  in (resolveWith roll critRoll atk def, gen2)

applyDamage :: Stats -> Damage -> Stats
applyDamage s (Damage d) = s { sHP = max 0 (sHP s - d) }

isDead :: Stats -> Bool
isDead s = sHP s <= 0

resultDamage :: CombatResult -> Int
resultDamage Miss                     = 0
resultDamage (Hit         (Damage d)) = d
resultDamage (CriticalHit (Damage d)) = d
resultDamage (Kill        (Damage d)) = d

-- | Message for the player attacking a named target.
describeAttack :: CombatResult -> String -> String
describeAttack Miss                     target = "You miss the " ++ target ++ "."
describeAttack (Hit         (Damage d)) target = "You hit the " ++ target ++ " for " ++ show d ++ "."
describeAttack (CriticalHit (Damage d)) target = "You crit the " ++ target ++ " for " ++ show d ++ "!"
describeAttack (Kill        (Damage d)) target = "You kill the " ++ target ++ " (" ++ show d ++ ")."

-- | Message for a named attacker hitting the player.
describeAttacked :: CombatResult -> String -> String
describeAttacked Miss                     attacker = "The " ++ attacker ++ " misses you."
describeAttacked (Hit         (Damage d)) attacker = "The " ++ attacker ++ " hits you for " ++ show d ++ "."
describeAttacked (CriticalHit (Damage d)) attacker = "The " ++ attacker ++ " crits you for " ++ show d ++ "!"
describeAttacked (Kill        (Damage d)) attacker = "The " ++ attacker ++ " kills you (" ++ show d ++ ")."

-- | Result of applying a hit to a monster. Returned by
--   'applyHitResult' so the caller can wire the fields back into
--   'GameState' without Combat needing to import it.
data HitOutcome = HitOutcome
  { hoMonsters     :: ![Monster]
  , hoPlayerStats  :: !Stats
  , hoRng          :: !StdGen
  , hoMessages     :: ![String]      -- ^ newest-first, to prepend
  , hoItemsOnFloor :: ![(Pos, Item)]
  , hoEvents       :: ![GameEvent]
  , hoVictory      :: !Bool
  , hoFinalTurns   :: !(Maybe Int)
  , hoQuests       :: ![Quest]
  }

-- | Shared post-resolution pipeline for any player-initiated hit
--   on a monster (melee or ranged). Pure — takes the specific
--   fields it needs and returns a 'HitOutcome'.
applyHitResult
  :: Int            -- ^ monster index
  -> Monster        -- ^ the monster hit
  -> CombatResult
  -> [String]       -- ^ caller-supplied messages (e.g. "You hit the rat for 3.")
  -> [Monster]      -- ^ current monster list
  -> Stats          -- ^ player stats
  -> StdGen         -- ^ RNG (already advanced past the attack roll)
  -> [(Pos, Item)]  -- ^ items on floor
  -> [String]       -- ^ current messages (newest-first)
  -> Bool           -- ^ current victory flag
  -> Maybe Int      -- ^ current final turns
  -> Int            -- ^ turns elapsed
  -> [Quest]        -- ^ current quests
  -> HitOutcome
applyHitResult i m result hitMsgs monsters playerStats gen
               itemsOnFloor msgs victory finalTurns turnsElapsed quests =
  let newMStats      = applyDamage (mStats m) (Damage (resultDamage result))
      killed         = isDead newMStats
      wasBoss        = isBoss (mKind m)
      combatEv       = playerCombatEvent result
      (playerStats', levelMsgs, levelEvs) =
        if killed
          then
            let reward     = P.xpReward (mKind m)
                (s', ups)  = P.gainXP playerStats reward
                startLevel = sLevel playerStats
                endLevel   = sLevel s'
                lvlMsgs = [ "You reach level " ++ show l ++ "!"
                           | l <- [endLevel, endLevel - 1 .. startLevel + 1]
                           ]
                evs  = replicate ups EvLevelUp
            in (s', lvlMsgs, evs)
          else (playerStats, [], [])
      monsters' =
        if killed
          then removeAt i monsters
          else updateAt i (\mo -> mo { mStats = newMStats }) monsters
      (loot, gen'') =
        if killed
          then Loot.rollLoot gen (mKind m)
          else ([], gen)
      lootMsgs =
        [ "The " ++ monsterName (mKind m) ++ " drops a " ++ itemName it ++ "."
        | it <- loot
        ]
      itemsOnFloor' =
        itemsOnFloor ++ [ (mPos m, it) | it <- loot ]
      bossEvs  = [ EvBossKilled | killed && wasBoss ]
      bossMsgs = [ "With a final roar, the " ++ monsterName (mKind m)
                   ++ " falls. You are victorious!"
                 | killed && wasBoss ]
      victory' = victory || (killed && wasBoss)
      finalTurns' = case finalTurns of
        Just _  -> finalTurns
        Nothing
          | victory' && not victory -> Just turnsElapsed
          | otherwise               -> Nothing
      allMsgs =
        reverse lootMsgs ++ bossMsgs ++ levelMsgs
          ++ reverse hitMsgs ++ msgs
      allEvs = combatEv : levelEvs ++ bossEvs
      -- Fire quest events on kill
      questEvs = if wasBoss then [EvKilledMonster, EvKilledBoss]
                            else [EvKilledMonster]
      (quests', questMsgs) =
        if killed
          then foldl (\(qs, ms) ev ->
                 let (qs', ms') = fireQuestEvent ev qs
                 in (qs', reverse ms' ++ ms)) (quests, []) questEvs
          else (quests, [])
  in HitOutcome
       { hoMonsters     = monsters'
       , hoPlayerStats  = playerStats'
       , hoRng          = gen''
       , hoMessages     = reverse questMsgs ++ allMsgs
       , hoItemsOnFloor = itemsOnFloor'
       , hoEvents       = allEvs
       , hoVictory      = victory'
       , hoFinalTurns   = finalTurns'
       , hoQuests       = quests'
       }

-- | Map a combat result to the event the *attacker* cares about
--   when the attacker is the player.
playerCombatEvent :: CombatResult -> GameEvent
playerCombatEvent Miss            = EvAttackMiss
playerCombatEvent (Hit _)         = EvAttackHit
playerCombatEvent (CriticalHit _) = EvAttackCrit
playerCombatEvent (Kill _)        = EvMonsterKilled

-- | Map a combat result to the event for the player being hit.
--   'Nothing' means "no sound for this" (we skip monster whiffs).
monsterCombatEvent :: CombatResult -> Maybe GameEvent
monsterCombatEvent Miss            = Nothing
monsterCombatEvent (Hit _)         = Just EvPlayerHurt
monsterCombatEvent (CriticalHit _) = Just EvPlayerHurt
monsterCombatEvent (Kill _)        = Just EvPlayerDied