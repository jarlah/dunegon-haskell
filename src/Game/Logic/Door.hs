-- | Door operations and dash-step calculation. All functions operate
--   on 'DungeonLevel' directly — no 'GameState' dependency.
module Game.Logic.Door
  ( openDoorAt
  , closeDoorAt
  , dashSteps
  ) where

import qualified Data.Vector as V
import Linear (V2(..))

import Game.Types
  ( Pos, Dir, DungeonLevel(..), Tile(..), DoorState(..), Item
  , Monster, tileAt, dirToOffset
  )
import Game.State.Types (NPC(..))
import Game.Logic.Lookup (monsterAt, npcAt)

-- | Rewrite the tile at 'p' to @Door Open@. Used by the bump-to-open
--   path in 'applyAction' so a closed door becomes walkable on the
--   same turn the player tried to step onto it. Out-of-bounds
--   positions are a no-op (the caller only invokes this after
--   'tileAt' has already confirmed the tile is @Door Closed@).
openDoorAt :: Pos -> DungeonLevel -> DungeonLevel
openDoorAt (V2 x y) lvl =
  let w = dlWidth  lvl
      h = dlHeight lvl
  in if x < 0 || y < 0 || x >= w || y >= h
       then lvl
       else let idx      = y * w + x
                newTiles = dlTiles lvl V.// [(idx, Door Open)]
            in lvl { dlTiles = newTiles }

-- | Rewrite the tile at 'p' to @Door Closed@. Mirrors 'openDoorAt'
--   and is used by 'playerCloseDoor'. The caller is responsible
--   for confirming the tile is currently @Door Open@ and that
--   nothing stands on it.
closeDoorAt :: Pos -> DungeonLevel -> DungeonLevel
closeDoorAt (V2 x y) lvl =
  let w = dlWidth  lvl
      h = dlHeight lvl
  in if x < 0 || y < 0 || x >= w || y >= h
       then lvl
       else let idx      = y * w + x
                newTiles = dlTiles lvl V.// [(idx, Door Closed)]
            in lvl { dlTiles = newTiles }

-- | Accumulate up to @n@ successive positions in direction @dir@
--   starting from the given position. Stops as soon as the next step
--   would land on anything other than plain floor or an open door,
--   anything occupied by a monster / NPC, or anything with a floor
--   item on it. Returns the list of /positions stepped onto/ (empty
--   list = blocked at step 1).
dashSteps
  :: DungeonLevel
  -> [Monster]
  -> [NPC]
  -> [(Pos, Item)]
  -> Pos
  -> Dir
  -> Int
  -> [Pos]
dashSteps dl monsters npcs items playerPos dir n = go n playerPos
  where
    offset = dirToOffset dir

    go 0 _ = []
    go k p =
      let next = p + offset
      in if not (dashPassable next)
           then []
           else next : go (k - 1) next

    dashPassable p =
      case tileAt dl p of
        Just Floor       -> clearOfActors p
        Just (Door Open) -> clearOfActors p
        _                -> False

    clearOfActors p =
         case monsterAt p monsters of { Just _ -> False; Nothing -> True }
      && case npcAt     p npcs      of { Just _ -> False; Nothing -> True }
      && not (any ((== p) . fst) items)
