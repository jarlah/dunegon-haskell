module Game.Logic.MonsterAI
  ( MonsterIntent(..)
  , monsterIntent
  , chebyshev
  ) where

import Linear (V2(..))

import Game.Logic.FOV (hasLineOfSight)
import Game.Types

-- | What a monster wants to do this turn.
data MonsterIntent
  = MiWait
  | MiMove !Pos
  | MiAttack
  deriving (Eq, Show)

chebyshev :: Pos -> Pos -> Int
chebyshev (V2 x1 y1) (V2 x2 y2) = max (abs (x1 - x2)) (abs (y1 - y2))

-- | Decide what a monster should do this turn. Pure.
--
--   The monster first checks whether it can actually *see* the
--   player: any tile of its footprint is within @sightRadius@
--   (Euclidean, matching 'Game.Logic.FOV.computeFOV') and has a
--   clear line of sight to the player. If not, it waits in place —
--   closed doors, walls, and distance all hide the player
--   completely. This intentionally has no memory: as soon as the
--   player leaves LOS, the monster stops pursuing.
--
--   If any footprint tile is adjacent to the player (Chebyshev ≤ 1),
--   attack. Otherwise try to slide the /whole/ footprint one step
--   toward the player: every tile of the proposed new position must
--   be walkable, in-bounds, not overlap the player, and not overlap
--   any of 'blockedPositions' (which the caller assembles as the
--   union of every /other/ monster's tiles).
--
--   Movement decisions use the top-left of the footprint as the
--   canonical anchor — we pick an 8-way offset and apply it to
--   'mPos', then validate that every tile of the footprint is
--   legal at the new top-left.
monsterIntent
  :: DungeonLevel
  -> Pos          -- ^ player position
  -> [Pos]        -- ^ tiles occupied by other monsters' footprints
  -> Int          -- ^ sight radius
  -> Monster
  -> MonsterIntent
monsterIntent dl playerPos blockedPositions sightRadius m
  | not (canSeePlayer dl playerPos sightRadius m)           = MiWait
  | any (\t -> chebyshev t playerPos <= 1) (monsterTiles m) = MiAttack
  | otherwise =
      let footprint = mFootprint m
          here      = mPos m
          candidates =
            [ newTopLeft
            | d <- [N, NE, E, SE, S, SW, W, NW]
            , let newTopLeft = here + dirToOffset d
            , let newTiles   = offsetTiles newTopLeft footprint
            , all (canStandOn dl playerPos blockedPositions) newTiles
            ]
      in maybe MiWait MiMove (bestNeighbor playerPos candidates)

-- | Can this monster see the player from its current position?
--   Any footprint tile within the Euclidean sight radius AND with a
--   clear line of sight counts — multi-tile bosses are as perceptive
--   as their largest silhouette.
canSeePlayer :: DungeonLevel -> Pos -> Int -> Monster -> Bool
canSeePlayer dl playerPos sightRadius m =
  any seesFrom (monsterTiles m)
  where
    r2 = sightRadius * sightRadius
    seesFrom t =
      let V2 dx dy = playerPos - t
      in dx * dx + dy * dy <= r2
         && hasLineOfSight dl t playerPos

-- | All tiles covered by a footprint anchored at the given
--   top-left. Mirrors 'monsterTiles' but without needing a full
--   'Monster' value — useful for validating hypothetical moves.
offsetTiles :: Pos -> Pos -> [Pos]
offsetTiles (V2 x y) (V2 w h) =
  [ V2 (x + dx) (y + dy) | dy <- [0 .. h - 1], dx <- [0 .. w - 1] ]

-- | Is this single tile a legal place for (some part of) a
--   monster to stand? Walkable terrain, in bounds, not the
--   player, and not overlapping another monster's footprint.
canStandOn :: DungeonLevel -> Pos -> [Pos] -> Pos -> Bool
canStandOn dl playerPos blockedSet p =
  p /= playerPos
    && p `notElem` blockedSet
    && maybe False isWalkable (tileAt dl p)

-- | Pick whichever candidate position minimizes Chebyshev
--   distance to the target. Prefers the first argument on ties.
bestNeighbor :: Pos -> [Pos] -> Maybe Pos
bestNeighbor target = foldr pick Nothing
  where
    pick p Nothing  = Just p
    pick p (Just b)
      | chebyshev p target < chebyshev b target = Just p
      | otherwise                               = Just b
