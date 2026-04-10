-- | Per-turn tick functions. Each takes the specific fields it needs
--   and returns updated values — no 'GameState' dependency.
module Game.Logic.Tick
  ( tickDash
  , tickRegen
  , tickTurnCounter
  ) where

import qualified Data.Set as Set
import Data.Set (Set)

import Game.Types (Pos, Stats(..), Monster, monsterTiles)
import Game.Logic.Constants (regenInterval)

-- | Decrement dash cooldown by one if positive, otherwise no-op.
tickDash :: Int -> Int
tickDash cd
  | cd > 0    = cd - 1
  | otherwise = cd

-- | Passive HP regen tick. Returns @(newStats, newRegenCounter)@.
--
--   While no hostile monster sits in the player's visible set,
--   accumulate one "safe turn" per call; on hitting 'regenInterval'
--   add 1 HP (capped at 'sMaxHP') and reset. A hostile becoming
--   visible immediately resets the counter.
--
--   Early-outs: full HP, dead, or victorious → no regen.
tickRegen
  :: Bool       -- ^ dead?
  -> Bool       -- ^ victory?
  -> Stats      -- ^ player stats
  -> Int        -- ^ current regen counter
  -> Set Pos    -- ^ currently visible tiles
  -> [Monster]  -- ^ all monsters on this level
  -> (Stats, Int)
tickRegen dead victory stats counter vis monsters
  | dead    = (stats, counter)
  | victory = (stats, counter)
  | sHP stats >= sMaxHP stats = (stats, 0)
  | hostileVisible            = (stats, 0)
  | otherwise =
      let next = counter + 1
      in if next >= regenInterval
           then (stats { sHP = min (sMaxHP stats) (sHP stats + 1) }, 0)
           else (stats, next)
  where
    hostileVisible = any (\m -> any (`Set.member` vis) (monsterTiles m)) monsters

-- | Advance the turn counter by one, unless dead or victory-frozen.
tickTurnCounter :: Bool -> Maybe Int -> Int -> Int
tickTurnCounter dead finalTurns turnsElapsed
  | dead              = turnsElapsed
  | Just _ <- finalTurns = turnsElapsed
  | otherwise         = turnsElapsed + 1
