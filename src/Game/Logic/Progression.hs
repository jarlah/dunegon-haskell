module Game.Logic.Progression
  ( xpReward
  , xpForNextLevel
  , gainXP
  , levelUp
  ) where

import Game.Types (Stats(..), MonsterKind(..))

-- | XP awarded for killing a monster of the given kind. Bosses
--   only pay a symbolic XP bounty — the real payoff for a boss
--   kill comes from turning in the matching quest.
xpReward :: MonsterKind -> Int
xpReward Rat    = 5
xpReward Goblin = 15
xpReward Orc    = 40
xpReward Dragon = 100

-- | XP required to advance *from* the given level to the next one.
--   A quadratic curve: 25, 100, 225, 400, 625, ...
xpForNextLevel :: Int -> Int
xpForNextLevel lvl = 25 * max 1 lvl * max 1 lvl

-- | Apply a single level-up: bump level, grow the HP pool, tune stats,
--   and fully heal the player. Does NOT touch sXP — the caller ('gainXP')
--   manages the XP carry-over across thresholds.
levelUp :: Stats -> Stats
levelUp s =
  let newMax = sMaxHP s + 5
  in s
       { sLevel   = sLevel s + 1
       , sMaxHP   = newMax
       , sHP      = newMax           -- full heal on level up
       , sAttack  = sAttack s + 1
       , sDefense = sDefense s + 1
       }

-- | Add XP to a stat block, applying as many level-ups as the pile
--   supports. Returns the new stats and the number of level-ups that
--   happened (useful for user messaging).
gainXP :: Stats -> Int -> (Stats, Int)
gainXP s xp
  | xp <= 0   = (s, 0)
  | otherwise =
      let total     = sXP s + xp
          threshold = xpForNextLevel (sLevel s)
      in if total >= threshold
           then
             let leveled        = (levelUp s) { sXP = 0 }
                 carry          = total - threshold
                 (final, rest)  = gainXP leveled carry
             in (final, 1 + rest)
           else (s { sXP = total }, 0)
