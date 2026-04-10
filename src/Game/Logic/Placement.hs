-- | Level placement helpers: flood-fill reachability, key/chest/boss
--   placement. All functions are pure — they thread 'StdGen'
--   explicitly and never touch 'GameState'.
module Game.Logic.Placement
  ( spawnSideReachable
  , placeKeyLoot
  , placeChests
  , roomPositions
  , pickBossTopLeft
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Linear (V2(..))
import System.Random (StdGen, randomR)

import Game.Types (Pos, DungeonLevel, Item(..), KeyId, Tile(..), DoorState(..), tileAt)
import Game.Logic.Chest (Chest(..), ChestState(..))
import qualified Game.Logic.Chest as Chest
import qualified Game.Logic.Dungeon as D

-- | Flood-fill from the spawn position, treating locked doors as
--   walls (and everything else — including open/closed doors — as
--   passable, since bump-to-open doesn't need a key). The result is
--   exactly the set of tiles a keyless player can reach from their
--   spawn tile. At most one locked door exists per level, so this is
--   equivalent to "reachable while the lock stays locked."
--
--   Used by 'placeKeyLoot' to guarantee a minted key is never placed
--   on the far side of its own lock (which would softlock the run).
spawnSideReachable :: DungeonLevel -> Pos -> Set Pos
spawnSideReachable dl start = go (Set.singleton start) [start]
  where
    passable p = case tileAt dl p of
      Just Wall           -> False
      Just (Door (Locked _)) -> False
      Just _              -> True
      Nothing             -> False

    go visited []       = visited
    go visited (p : qs) =
      let neighbors =
            [ p + V2 0 (-1)
            , p + V2 0   1
            , p + V2 1   0
            , p + V2 (-1) 0
            ]
          fresh =
            [ n
            | n <- neighbors
            , not (Set.member n visited)
            , passable n
            ]
      in go (foldr Set.insert visited fresh) (qs ++ fresh)

-- | Place each key in a randomly chosen reachable room tile, as a
--   @(Pos, IKey kid)@ entry suitable for 'gsItemsOnFloor'.
--
--   The reachability filter is what prevents a softlock where the
--   lock sits between spawn and the key itself.
placeKeyLoot
  :: StdGen
  -> Set Pos
  -> [D.Room]
  -> [KeyId]
  -> ([(Pos, Item)], StdGen)
placeKeyLoot gen0 reachable rooms keys =
  let -- Tiles of a room that are actually reachable (pre-filtered).
      roomReachableTiles r =
        [ p | p <- roomPositions r, Set.member p reachable ]

      -- Rooms that still have at least one reachable tile.
      reachableRooms = filter (not . null . roomReachableTiles) rooms

      -- Prefer non-spawn rooms (index >= 1). If none of those are
      -- reachable (pathological: the locked door seals off
      -- everything past the spawn room), fall back to the spawn
      -- room alone. If nothing is reachable at all, the key is
      -- silently dropped — at that point the level is broken in
      -- ways unrelated to locked doors.
      preferredRooms =
        let nonSpawn = filter (not . null . roomReachableTiles) (drop 1 rooms)
        in if null nonSpawn
             then case rooms of
                    (r : _) | not (null (roomReachableTiles r)) -> [r]
                    _                                            -> []
             else nonSpawn

      pool
        | not (null preferredRooms) = preferredRooms
        | otherwise                 = reachableRooms

      go g [] = ([], g)
      go g (k : ks) = case pool of
        [] -> go g ks  -- nothing reachable: drop the key
        _  ->
          let (ri, g1)    = randomR (0, length pool - 1) g
              room        = pool !! ri
              tiles       = roomReachableTiles room
              (ti, g2)    = randomR (0, length tiles - 1) g1
              pos         = tiles !! ti
              (rest, g3)  = go g2 ks
          in ((pos, IKey k) : rest, g3)
  in go gen0 keys

-- | Place up to @n@ full chests in the given candidate rooms,
--   avoiding any tile already occupied by a monster (per footprint),
--   item, NPC, another chest, or terrain that isn't a plain floor
--   (stairs, doors, walls). Each chest is seeded with a freshly
--   rolled item from 'Chest.rollChestLoot', so newly generated
--   floors show a full chest until the player bumps it.
--
--   The RNG is threaded through the picks and the loot rolls so
--   the placement is save/load-deterministic. Returns as many
--   chests as could be placed — if every candidate tile is
--   occupied, fewer than @n@ (possibly zero) chests are returned.
placeChests
  :: StdGen
  -> DungeonLevel
  -> [D.Room]
  -> [Pos]
  -> Int
  -> ([Chest], StdGen)
placeChests gen0 dl rooms occupied n
  | n <= 0 || null rooms = ([], gen0)
  | otherwise =
      let candidates =
            [ p
            | r <- rooms
            , p <- roomPositions r
            , case tileAt dl p of
                Just Floor -> True
                _          -> False
            , p `notElem` occupied
            ]
      in pick gen0 candidates n
  where
    pick g _    0 = ([], g)
    pick g []   _ = ([], g)
    pick g cs   k =
      let (i, g1)     = randomR (0, length cs - 1) g
          pos         = cs !! i
          rest        = take i cs ++ drop (i + 1) cs
          (item, g2)  = Chest.rollChestLoot g1
          chest       = Chest { chestPos = pos, chestState = ChestFull item }
          (more, g3)  = pick g2 rest (k - 1)
      in (chest : more, g3)

-- | Every @(x, y)@ floor tile inside a room's rectangle. Rooms are
--   carved as solid floor, so every position here is guaranteed to
--   be a walkable tile in the generated level.
roomPositions :: D.Room -> [Pos]
roomPositions r =
  [ V2 x y
  | x <- [D.rX r .. D.rX r + D.rW r - 1]
  , y <- [D.rY r .. D.rY r + D.rH r - 1]
  ]

-- | Pick a random top-left position inside a room such that a 2x2
--   footprint fits entirely within the room's interior. For a room
--   with width or height of exactly 1 (shouldn't happen given
--   'lcRoomMin' = 4) this degenerates to the top-left corner.
pickBossTopLeft :: D.Room -> StdGen -> (Pos, StdGen)
pickBossTopLeft r gen0 =
  let xMax    = D.rX r + max 0 (D.rW r - 2)
      yMax    = D.rY r + max 0 (D.rH r - 2)
      (x, g1) = randomR (D.rX r, xMax) gen0
      (y, g2) = randomR (D.rY r, yMax) g1
  in (V2 x y, g2)
