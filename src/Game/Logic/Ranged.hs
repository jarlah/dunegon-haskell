-- | Pure helpers for ranged attacks. The actual 'fireArrow'
--   transform lives in "Game.GameState" next to 'playerAttack' and
--   'playerDash' — it has to touch almost every field of
--   'GameState', so co-locating it with the other player actions
--   keeps the pattern consistent and avoids an import cycle.
--
--   This module owns two things that are pure and don't need a
--   'GameState' dependency:
--
--     * 'arrowRange' — the per-shot tile cap.
--     * 'RayOutcome' and 'walkRay' — the tile-by-tile raycast
--       that decides whether an arrow flies past, thuds into a
--       wall, is absorbed by an NPC or chest, or lands on a
--       monster.
module Game.Logic.Ranged
  ( arrowRange
  , RayOutcome (..)
  , walkRay
  ) where

import Game.Types

-- | How many tiles an arrow travels before falling harmlessly.
--   Tuned to be comfortably more than a single corridor's worth
--   but well short of a whole-room diagonal so bow play feels
--   different from melee without trivializing approach tactics.
arrowRange :: Int
arrowRange = 8

-- | Possible outcomes of tracing an arrow's path. The string
--   carried by 'RayBlocked' is the tail of a message — the caller
--   stitches it onto "Your arrow " to form a full line.
data RayOutcome
  = RayHitMonster !Int !Monster
  | RayBlocked    !String
  | RayDropped
  deriving (Eq, Show)

-- | Walk a precomputed list of tiles in flight order and return
--   the first stopping event. Caller supplies:
--
--     * the dungeon level (for terrain),
--     * the monster list (and a monster-at-pos lookup),
--     * the NPC list (treated as arrow-stopping friendly fire),
--     * the chest list (treated as solid world objects),
--     * the precomputed path.
--
--   Taking each collection as a plain argument keeps this module
--   free of any 'GameState' dependency and lets the existing
--   unit test harness exercise it against hand-built fixtures.
walkRay
  :: DungeonLevel
  -> (Pos -> Maybe (Int, Monster))
  -> (Pos -> Bool)   -- ^ NPC present at position?
  -> (Pos -> Bool)   -- ^ chest present at position?
  -> [Pos]
  -> RayOutcome
walkRay _  _        _      _        [] = RayDropped
walkRay dl monsterL npcHit chestHit (p : ps) =
  case tileAt dl p of
    Nothing   -> RayBlocked "clatters off into the void"
    Just Wall -> RayBlocked "clatters against the wall"
    Just (Door Closed)     -> RayBlocked "thuds into the closed door"
    Just (Door (Locked _)) -> RayBlocked "thuds into the locked door"
    _
      | npcHit p   -> RayBlocked "whistles past a friendly face"
      | chestHit p -> RayBlocked "strikes a chest with a dull thunk"
      | otherwise  -> case monsterL p of
          Just (i, m) -> RayHitMonster i m
          Nothing     -> walkRay dl monsterL npcHit chestHit ps
