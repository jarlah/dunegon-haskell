module Game.Logic.Constants
  ( fovRadius
  , monsterSightRadius
  , regenInterval
  , defaultPlayerStats
  , dashMaxSteps
  , dashCooldownTurns
  , launchOptions
  ) where

import Game.Types (Stats(..))
import Game.State.Types (LaunchOption(..))

-- | The fixed order of options on the launch screen. Kept as a
--   top-level list so the renderer and the key handler agree on
--   indices without having to duplicate the list.
launchOptions :: [LaunchOption]
launchOptions = [LaunchNewGame, LaunchContinue, LaunchLoad, LaunchQuit]

-- | How far a dash moves the player, in tiles, before stopping on
--   any obstacle (wall, closed/locked door, monster, NPC, item, or
--   stairs). Five is enough to break line of sight against most
--   regular monsters' 8-tile sight radius after two dashes, which
--   is the scenario the mechanic exists to support.
dashMaxSteps :: Int
dashMaxSteps = 5

-- | How many turns must pass after a dash before the player can
--   dash again. Ticks in 'processMonsters', which runs once per
--   turn-advancing action.
dashCooldownTurns :: Int
dashCooldownTurns = 60

-- | How many consecutive "safe" turns (no hostile monster visible
--   in the player's FOV) the player must accumulate before
--   regenerating 1 HP via 'tickRegen'. Twelve feels slow enough
--   that potions still matter in active combat but fast enough
--   that retreating behind a closed door actually pays off.
regenInterval :: Int
regenInterval = 12

-- | How far the player can see, in tiles. Measured in Euclidean
--   distance; 8 feels right for a 60×20 dungeon.
fovRadius :: Int
fovRadius = 8

-- | How far a monster can see, in tiles. Kept symmetric with
--   'fovRadius' so "if I can see it, it can see me" — Milestone 16
--   can retune if playtesting shows the player needs a scouting
--   advantage. Measured in Euclidean distance, matching the FOV.
monsterSightRadius :: Int
monsterSightRadius = 8

defaultPlayerStats :: Stats
defaultPlayerStats = Stats
  { sHP      = 25
  , sMaxHP   = 25
  , sAttack  = 6
  , sDefense = 2
  , sSpeed   = 10
  , sLevel   = 1
  , sXP      = 0
  }
