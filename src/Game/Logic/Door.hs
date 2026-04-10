-- | Door open/close operations.
module Game.Logic.Door
  ( openDoorAt
  , closeDoorAt
  ) where

import qualified Data.Vector as V
import Linear (V2(..))

import Game.Types (DungeonLevel(..), Pos, Tile(..), DoorState(..))

openDoorAt :: Pos -> DungeonLevel -> DungeonLevel
openDoorAt = setDoorAt Open

closeDoorAt :: Pos -> DungeonLevel -> DungeonLevel
closeDoorAt = setDoorAt Closed

-- | Rewrite the tile at 'p' to @Door state@. Out-of-bounds
--   positions are a no-op.
setDoorAt :: DoorState -> Pos -> DungeonLevel -> DungeonLevel
setDoorAt state (V2 x y) lvl =
  let w = dlWidth  lvl
      h = dlHeight lvl
  in if x < 0 || y < 0 || x >= w || y >= h
       then lvl
       else let idx      = y * w + x
                newTiles = dlTiles lvl V.// [(idx, Door state)]
            in lvl { dlTiles = newTiles }
