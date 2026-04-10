-- | Run ranking / scoring. Pure function that computes a textual
--   rank label from the run's final stats.
module Game.Logic.Scoring
  ( runRank
  ) where

-- | Compute a textual "rank" label for a finished run, based on
--   the three gamified counters: how many turns the boss kill
--   took, how many potions were burned, and how many times the
--   player saved. Lower is better on every axis.
--
--   Tiers (from strictest to loosest):
--     * /Legendary/ — boss in ≤ 1500 turns, ≤ 3 potions, 0 saves.
--     * /Heroic/ — boss in ≤ 2500 turns, ≤ 6 potions, ≤ 2 saves.
--     * /Victor/ — you finished.
runRank
  :: Maybe Int  -- ^ final turns (Nothing = in-progress or dead)
  -> Bool       -- ^ dead?
  -> Int        -- ^ potions used
  -> Int        -- ^ saves used
  -> String
runRank finalTurns dead potionsUsed savesUsed = case finalTurns of
  Nothing -> if dead then "Fallen" else "In Progress"
  Just t
    | t <= 1500 && potionsUsed <= 3 && savesUsed == 0 -> "Legendary"
    | t <= 2500 && potionsUsed <= 6 && savesUsed <= 2 -> "Heroic"
    | otherwise                                       -> "Victor"
