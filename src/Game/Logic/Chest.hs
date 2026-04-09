-- | Respawning loot chests (Milestone 17, Step 1B).
--
--   A chest is a pure-data world object that lives in 'gsChests' on
--   'GameState' (and 'plChests' when a floor is parked). Each chest
--   has a 'ChestState' that alternates between 'ChestFull' (carrying
--   exactly one 'Item') and 'ChestEmpty' with a refill countdown.
--
--   The mechanic exists to let /time/ be a resource: a patient
--   player who retreats, breaks line of sight, and waits out the
--   cooldown can always farm another potion. That supports the
--   dash-and-door tactical retreat loop introduced in Milestone 16
--   without the player having to scum monster drops for sustain.
--
--   This module is deliberately pure and backend-agnostic: no IO,
--   no 'GameState' dependency, no rendering. It exposes the types,
--   the loot table, the refill constant, and a couple of small
--   helpers. Wiring into 'GameState' (field, tick hook, bump-to-
--   open, render) lives in "Game.GameState" and "Game.Render".
module Game.Logic.Chest
  ( -- * Types
    Chest(..)
  , ChestState(..)
    -- * Configuration
  , chestRespawnTurns
  , chestLootTable
    -- * Rolls and ticks
  , rollChestLoot
  , tickChest
  , refillChests
  ) where

import System.Random (StdGen)

import Game.Logic.Loot (pickWeighted)
import Game.Types

-- | The two states a chest can be in.
--
--   * 'ChestFull' holds exactly one 'Item'. Bumping the chest
--     transfers the item into the player's bag (or drops it onto
--     the floor if the bag is full) and flips the chest to
--     'ChestEmpty chestRespawnTurns'.
--
--   * 'ChestEmpty n' is the cooling-down state. The @n@ is how
--     many more 'tickPlayerTurn' calls the chest must survive
--     before it is eligible to refill. Refills only happen on
--     floor re-entry — parked floors don't tick, so the wait
--     effectively starts the moment the player leaves the floor.
data ChestState
  = ChestFull !Item
  | ChestEmpty !Int
  deriving (Eq, Show)

-- | A single chest on a dungeon floor. Chests are placed by the
--   generator at level creation / re-entry; 'chestPos' never
--   changes over the life of a chest.
data Chest = Chest
  { chestPos   :: !Pos
  , chestState :: !ChestState
  } deriving (Eq, Show)

-- | How many turns an empty chest must sit before it is eligible
--   to refill on the next floor entry. One hundred is long enough
--   that the player can't stand next to a chest and farm it, but
--   short enough that a round trip up and down the stairs is
--   usually enough to reset it.
chestRespawnTurns :: Int
chestRespawnTurns = 100

-- | Weighted candidate list for a fresh chest. Weighted toward
--   minor healing potions (the whole point of sustain is to let
--   the player top up HP on their own schedule), with a smaller
--   chance at a major potion and the occasional weapon or armor
--   upgrade.
--
--   Weights don't have to sum to anything in particular — the
--   weighted picker walks the list and picks proportionally.
chestLootTable :: [(Int, Item)]
chestLootTable =
  [ (6, IPotion HealingMinor)
  , (2, IPotion HealingMajor)
  , (1, IWeapon ShortSword)
  , (1, IArmor  LeatherArmor)
  , (1, IWeapon Bow)
  , (2, IArrows 6)
  ]

-- | Roll a single loot item out of the chest table. Deterministic
--   given a fixed 'StdGen' seed. Falls back to a minor potion if
--   the table is somehow empty — defensive, the module constants
--   guarantee the happy path.
rollChestLoot :: StdGen -> (Item, StdGen)
rollChestLoot gen0 = case pickWeighted gen0 chestLootTable of
  Just (item, gen1) -> (item, gen1)
  Nothing           -> (IPotion HealingMinor, gen0)

-- | One turn of chest decay. 'ChestEmpty' counts down toward 0
--   (clamped, so a stale counter can't go negative through some
--   other code path). 'ChestFull' is a no-op — a live chest does
--   not rot.
tickChest :: Chest -> Chest
tickChest c = case chestState c of
  ChestFull _  -> c
  ChestEmpty n -> c { chestState = ChestEmpty (max 0 (n - 1)) }

-- | Walk a chest list and re-roll any chest whose cooldown has
--   expired (@ChestEmpty n@ with @n <= 0@), threading a single
--   RNG through the whole list. Called on floor re-entry after
--   a 'ParkedLevel' is restored — parked floors don't tick, so
--   the refill check happens the moment the player steps back
--   onto the floor.
--
--   Chests that are still 'ChestFull' or still counting down
--   pass through unchanged.
refillChests :: StdGen -> [Chest] -> ([Chest], StdGen)
refillChests gen0 = go gen0
  where
    go g []       = ([], g)
    go g (c : cs) = case chestState c of
      ChestEmpty n | n <= 0 ->
        let (item, g1) = rollChestLoot g
            refilled   = c { chestState = ChestFull item }
            (rest, g2) = go g1 cs
        in (refilled : rest, g2)
      _ ->
        let (rest, g1) = go g cs
        in (c : rest, g1)
