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
import Game.Utils.List (findIndexed)

-- | Find a monster occupying the given tile, if any. Uses
--   'monsterOccupies' so that multi-tile bosses resolve on any
--   tile of their footprint.
monsterAt :: Pos -> [Monster] -> Maybe (Int, Monster)
monsterAt p = findIndexed (`monsterOccupies` p)

-- | Index of the first 'IKey' in the player's bag whose 'KeyId'
--   matches the given lock, or 'Nothing' if no matching key is
--   present.
findKeyIndex :: KeyId -> Inventory -> Maybe Int
findKeyIndex kid inv = fst <$> findIndexed isMatchingKey (invItems inv)
  where
    isMatchingKey (IKey k) = k == kid
    isMatchingKey _        = False

-- | Index lookup for NPCs by position.
npcAt :: Pos -> [NPC] -> Maybe (Int, NPC)
npcAt p = findIndexed (\n -> npcPos n == p)

-- | Index lookup for chests by position.
chestAt :: Pos -> [Chest] -> Maybe (Int, Chest)
chestAt p = findIndexed (\c -> chestPos c == p)

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
