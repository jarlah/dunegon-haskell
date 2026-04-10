-- | Pure list-search helpers for finding monsters, NPCs, chests,
--   keys, and items by position or index. None of these touch
--   'GameState' — they operate on the plain lists the caller
--   extracts from the record.
module Game.Logic.Lookup
  ( monsterAt
  , npcAt
  , chestAt
  , replaceChestAt
  , findKeyIndex
  , takeFirstItemAt
  ) where

import Game.Types (Pos, Monster(..), KeyId, Inventory(..), Item(..), monsterOccupies)
import Game.State.Types (NPC(..))
import Game.Logic.Chest (Chest(..))

-- | Find a monster occupying the given tile, if any. Uses
--   'monsterOccupies' so that multi-tile bosses resolve on any
--   tile of their footprint — attacks and collisions that hit any
--   tile of a dragon all point back at the same 'Monster' entry.
monsterAt :: Pos -> [Monster] -> Maybe (Int, Monster)
monsterAt p = go 0
  where
    go _ [] = Nothing
    go i (m : rest)
      | monsterOccupies m p = Just (i, m)
      | otherwise           = go (i + 1) rest

-- | Index of the first 'IKey' in the player's bag whose 'KeyId'
--   matches the given lock, or 'Nothing' if no matching key is
--   present. Used by the bump-to-unlock path to consume the key
--   from the same position it was picked up from.
findKeyIndex :: KeyId -> Inventory -> Maybe Int
findKeyIndex kid inv = go 0 (invItems inv)
  where
    go _ []                       = Nothing
    go i (IKey k : rest) | k == kid = Just i
                         | otherwise = go (i + 1) rest
    go i (_      : rest)            = go (i + 1) rest

-- | Index lookup mirroring 'monsterAt' but for NPCs.
npcAt :: Pos -> [NPC] -> Maybe (Int, NPC)
npcAt p = go 0
  where
    go _ [] = Nothing
    go i (n : rest)
      | npcPos n == p = Just (i, n)
      | otherwise     = go (i + 1) rest

-- | Index lookup mirroring 'monsterAt' but for chests. Returns the
--   first chest whose 'chestPos' matches, along with its position
--   in 'gsChests'. Used by the bump-to-open path in 'applyAction'.
chestAt :: Pos -> [Chest] -> Maybe (Int, Chest)
chestAt p = go 0
  where
    go _ [] = Nothing
    go i (c : rest)
      | chestPos c == p = Just (i, c)
      | otherwise       = go (i + 1) rest

-- | Replace the chest at index @i@ with @c'@, leaving the rest of
--   the list untouched. Total over in-range indices; out-of-range
--   indices pass the list through unchanged (defensive — callers
--   use 'chestAt' to find the index in the first place).
replaceChestAt :: Int -> Chest -> [Chest] -> [Chest]
replaceChestAt i c' = go 0
  where
    go _ []       = []
    go j (c : cs)
      | j == i    = c' : cs
      | otherwise = c  : go (j + 1) cs

-- | Find and remove the first item at @p@ from the floor list,
--   preserving the order of the rest.
takeFirstItemAt :: Pos -> [(Pos, Item)] -> Maybe (Item, [(Pos, Item)])
takeFirstItemAt p = go []
  where
    go _    []                        = Nothing
    go seen ((q, it) : rest)
      | q == p    = Just (it, reverse seen ++ rest)
      | otherwise = go ((q, it) : seen) rest
