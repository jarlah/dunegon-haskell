# Haskell Roguelike — Project Plan

A terminal-based roguelike dungeon crawler built in Haskell, using **Apecs** for entity-component-system architecture, **Brick** for terminal UI, and **QuickCheck + HSpec** for comprehensive testing of pure game logic.

## Philosophy

The entire game logic layer is **pure Haskell** — no IO, no Apecs, no Brick. This means:

- Every game rule can be tested with QuickCheck property-based tests
- The rendering layer (Brick) is a thin, swappable shell
- The ECS layer (Apecs) bridges pure logic and the real world
- Type safety catches rule violations at compile time, not at runtime

---

## Stack

| Concern | Library | Role |
|---------|---------|------|
| ECS | `apecs` | Entity management, component storage |
| Terminal UI | `brick` + `vty` | Rendering, input handling |
| Math | `linear` | `V2 Int` for grid positions |
| Testing | `hspec`, `QuickCheck` | Unit + property-based tests |
| Randomness | `MonadRandom` | Procedural generation, combat rolls |
| Containers | `containers`, `vector` | Maps, sets, dungeon grids |
| Configuration | `yaml` | `config.yaml` parsing via Data.Yaml |
| HTTP | `http-tower-hs` | Resilient HTTP client (retries, timeouts, circuit-breaking) for AI features |

### Why Brick over SDL2

- Zero rendering complexity — focus on game logic and Haskell learning
- Terminal is the native medium for roguelikes
- Swap to SDL2 later by replacing only `Render.hs` and `Input.hs`
- The pure logic layer (80% of code) stays untouched

---

## Architecture

```
┌──────────────────────────────────────────┐
│  Main.hs                                 │
│  Brick app setup, event loop             │
│  Delegates everything downward           │
├──────────────────────────────────────────┤
│  UI Layer (IO, Brick-aware)              │
│  Render.hs  — GameState → Brick Widget   │
│  Input.hs   — Brick Event → GameAction   │
├──────────────────────────────────────────┤
│  ECS Layer (Apecs Systems, IO)           │
│  Systems.hs — runs pure logic on world   │
│  World.hs   — component defs, init       │
├──────────────────────────────────────────┤
│  Pure Logic Layer (NO IO)                │
│  Combat.hs, Movement.hs, FOV.hs,        │
│  Dungeon.hs, Inventory.hs, Quest.hs     │
│  ALL tested with QuickCheck + HSpec      │
└──────────────────────────────────────────┘
```

### Key principle: data flows down, decisions flow up

1. Brick captures a keypress → `Input.hs` maps it to a `GameAction`
2. `Systems.hs` reads relevant components from the Apecs world
3. Calls a pure function from `Logic/*` with the component data
4. Gets back a pure result (new positions, damage, items, etc.)
5. Writes the results back into the Apecs world
6. `Render.hs` reads the world and produces a `Widget`

---

## Module Layout

```
dungeon-haskell/
├── dungeon-haskell.cabal
├── app/
│   └── Main.hs                    -- Brick app entry point
├── src/
│   └── Game/
│       ├── Types.hs               -- Core types: Pos, Dir, Tile, etc.
│       ├── Components.hs          -- Apecs components
│       ├── World.hs               -- makeWorldAndComponents, initWorld
│       ├── Systems.hs             -- Apecs systems (thin IO bridge)
│       ├── Render.hs              -- Brick widget rendering
│       ├── Input.hs               -- Brick event → GameAction
│       └── Logic/
│           ├── Combat.hs          -- PURE: damage, attack, death
│           ├── Movement.hs        -- PURE: collision, can-move
│           ├── FOV.hs             -- PURE: field of view / visibility
│           ├── Dungeon.hs         -- PURE: procedural cave/room gen
│           ├── Inventory.hs       -- PURE: items, weight, equip
│           ├── Loot.hs            -- PURE: loot tables, drops
│           └── Quest.hs           -- PURE: quest state machine
├── test/
│   ├── Spec.hs                    -- Test runner (hspec-discover)
│   └── Game/Logic/
│       ├── CombatSpec.hs
│       ├── MovementSpec.hs
│       ├── FOVSpec.hs
│       ├── DungeonSpec.hs
│       ├── InventorySpec.hs
│       └── QuestSpec.hs
└── README.md
```

---

## Core Types (Game/Types.hs)

These are the shared vocabulary — pure data types, no dependencies on Apecs or Brick.

```haskell
module Game.Types where

import Linear (V2(..))

-- Grid position (column, row)
type Pos = V2 Int

-- Cardinal + diagonal directions
data Dir = N | NE | E | SE | S | SW | W | NW
  deriving (Eq, Show, Enum, Bounded)

dirToOffset :: Dir -> Pos
dirToOffset N  = V2 0 (-1)
dirToOffset NE = V2 1 (-1)
dirToOffset E  = V2 1 0
dirToOffset SE = V2 1 1
dirToOffset S  = V2 0 1
dirToOffset SW = V2 (-1) 1
dirToOffset W  = V2 (-1) 0
dirToOffset NW = V2 (-1) (-1)

-- Dungeon tile
data Tile = Floor | Wall | Door DoorState | StairsDown | StairsUp
  deriving (Eq, Show)

data DoorState = Open | Closed
  deriving (Eq, Show)

-- The dungeon grid
data DungeonLevel = DungeonLevel
  { dlWidth  :: !Int
  , dlHeight :: !Int
  , dlTiles  :: !(Vector Tile)   -- row-major
  , dlDepth  :: !Int             -- floor number
  } deriving (Eq, Show)

tileAt :: DungeonLevel -> Pos -> Maybe Tile
tileAt dl (V2 x y)
  | x < 0 || y < 0 || x >= dlWidth dl || y >= dlHeight dl = Nothing
  | otherwise = Just $ dlTiles dl ! (y * dlWidth dl + x)

isWalkable :: Tile -> Bool
isWalkable Floor      = True
isWalkable (Door Open)= True
isWalkable StairsDown = True
isWalkable StairsUp   = True
isWalkable _          = False

-- Stats
data Stats = Stats
  { sHP       :: !Int
  , sMaxHP    :: !Int
  , sAttack   :: !Int
  , sDefense  :: !Int
  , sSpeed    :: !Int
  } deriving (Eq, Show)

-- Items
data ItemKind = Weapon | Armor | Potion | Scroll | Gold
  deriving (Eq, Show, Enum, Bounded)

data Item = Item
  { iName   :: !String
  , iKind   :: !ItemKind
  , iWeight :: !Int
  , iValue  :: !Int
  } deriving (Eq, Show)

-- Game actions (what the player or AI wants to do)
data GameAction
  = Move Dir
  | Attack Dir
  | PickUp
  | UseItem Int        -- inventory slot index
  | DropItem Int
  | OpenDoor Dir
  | Wait
  | GoDownStairs
  | GoUpStairs
  | Quit
  deriving (Eq, Show)
```

---

## Milestone Plan

### Milestone 1: Walking in a Room

**Goal:** Player `@` moves in a hardcoded room, walls block movement.

Modules to build:
- `Types.hs` — Pos, Dir, Tile, DungeonLevel
- `Components.hs` — Position, Player, BlocksMovement
- `World.hs` — init with hardcoded room
- `Logic/Movement.hs` — `tryMove :: DungeonLevel -> Pos -> Dir -> Maybe Pos`
- `Systems.hs` — movement system
- `Render.hs` — render grid + player glyph
- `Input.hs` — arrow keys / vi keys → Move Dir

Tests:
- `MovementSpec.hs`:
  - `prop_cantWalkThroughWalls`
  - `prop_moveAndBackReturnsToStart`
  - `prop_allDirectionsMoveExactlyOneStep`
  - `prop_stayInBounds`

**Deliverable:** Runnable terminal game, player walks around, 4+ property tests pass.

### Milestone 2: Procedural Dungeons

**Goal:** Random cave/room generation with corridors.

Modules:
- `Logic/Dungeon.hs` — `generateLevel :: StdGen -> LevelConfig -> DungeonLevel`
  - Room placement (random rects, overlap rejection)
  - Corridor carving (L-shaped connectors)
  - Cave areas (cellular automata)
  - Stairs placement

Tests:
- `DungeonSpec.hs`:
  - `prop_allRoomsReachable` — flood fill from stairs, every floor tile reachable
  - `prop_stairsExist` — each level has up+down stairs
  - `prop_wallsBorderLevel` — edge tiles are always walls
  - `prop_minimumRoomCount`
  - `prop_noOverlappingRooms`

### Milestone 3: Monsters and Combat

**Goal:** Monsters spawn, bump-to-attack, damage, death, death penalty.

Modules:
- `Components.hs` — add Health, CombatStats, Monster, Corpse
- `Logic/Combat.hs`:
  - `resolveAttack :: StdGen -> Stats -> Stats -> (CombatResult, StdGen)`
  - `CombatResult = Miss | Hit Damage | CriticalHit Damage | Kill`
  - `applyDamage :: Stats -> Damage -> Stats`
  - `isDead :: Stats -> Bool`
- `Systems.hs` — combat system, death system, monster AI (chase player)

Tests:
- `CombatSpec.hs`:
  - `prop_damageNeverNegative`
  - `prop_armorReducesDamage`
  - `prop_zeroHPMeansDead`
  - `prop_deadCannotAttack`
  - `prop_criticalHitMoreDamage`
  - `prop_highDefenseReducesHitChance`

### Milestone 4: Field of View

**Goal:** Only visible tiles rendered, fog of war for explored areas.

Modules:
- `Logic/FOV.hs`:
  - `computeFOV :: DungeonLevel -> Pos -> Int -> Set Pos`
  - Algorithm: shadowcasting (recursive) — 8 octants, pure
- `Components.hs` — add Visibility, Explored (per-level set of seen tiles)

Tests:
- `FOVSpec.hs`:
  - `prop_playerAlwaysVisible` — player pos always in FOV set
  - `prop_adjacentFloorVisible` — if standing in open room, adjacent floor visible
  - `prop_wallBlocksSight`
  - `prop_fovSymmetry` — if A sees B, B sees A
  - `prop_fovRadiusBound` — no visible tile beyond max radius

### Milestone 5: Items and Inventory

**Goal:** Items on floor, pick up, drop, equip, use potions.

Modules:
- `Components.hs` — add Inventory, OnFloor, Equipped
- `Logic/Inventory.hs`:
  - `canPickUp :: Inventory -> Item -> Bool` (weight limit)
  - `addItem :: Inventory -> Item -> Either InventoryError Inventory`
  - `removeItem :: Inventory -> Int -> (Item, Inventory)`
  - `usePotion :: Stats -> Potion -> Stats`
- `Logic/Loot.hs`:
  - `rollLoot :: StdGen -> MonsterKind -> Int -> ([Item], StdGen)`
  - Loot tables as pure data

Tests:
- `InventorySpec.hs`:
  - `prop_pickUpThenDropReturnsItem`
  - `prop_cantExceedWeightLimit`
  - `prop_potionNeverExceedsMaxHP`
  - `prop_equipWeaponChangesAttack`
  - `prop_inventoryCountNeverNegative`

### Milestone 6: Quests and Depth

**Goal:** Simple quest system, multiple dungeon levels, death penalty.

Modules:
- `Logic/Quest.hs`:
  - Quest as a state machine: `data QuestState = NotStarted | Active Progress | Completed | Failed`
  - `advanceQuest :: QuestState -> QuestEvent -> QuestState`
  - Types: kill quests, fetch quests, explore quests
- Death penalty: lose gold %, lose some items, respawn at level 1

Tests:
- `QuestSpec.hs`:
  - `prop_completedQuestStaysCompleted`
  - `prop_questProgressNeverNegative`
  - `prop_failedQuestCantAdvance`

### Milestone 7: Experience and Leveling

**Goal:** Killing monsters grants XP. Enough XP levels the player up: bigger HP pool, stronger attack, full heal.

This milestone is pure and can slot in any time after Milestone 3 — the only reason it's listed here is so the earlier milestones stay small and focused. Combat already knows when a `Kill` happens; we just need to feed that event into a progression system.

Modules:
- `Game/Types.hs` — extend `Stats` (or add a new `Progression` record) with:
  - `sLevel   :: !Int`
  - `sXP      :: !Int`   -- XP accumulated toward the next level
- `Logic/Progression.hs`:
  - `xpReward       :: MonsterKind -> Int`          -- e.g. Rat=5, Goblin=15, Orc=40
  - `xpForNextLevel :: Int -> Int`                  -- curve, e.g. `50 * level * level`
  - `gainXP         :: Stats -> Int -> (Stats, Int)`  -- returns new stats + number of level-ups
  - `levelUp        :: Stats -> Stats`              -- +maxHP, +attack, +defense, full heal
- `Logic/Combat.hs` — extend `CombatResult`:
  - `Kill Damage` keeps its damage, and the killer's `gainXP` is applied in `GameState` via the monster kind that was killed
  - OR add an XP field to `Kill`: `Kill Damage Int` — either works; whichever keeps Combat ignorant of monster kinds is preferred
- `GameState.hs` — when a player attack returns `Kill`, call `gainXP` on the player's stats, append a `"You reach level N!"` message for each level-up
- `Render.hs` — status bar shows `LVL 3   XP: 42/100   HP: ...`

Tests:
- `ProgressionSpec.hs`:
  - `prop_xpNeverNegative`                — `sXP` after `gainXP` is always ≥ 0
  - `prop_gainXPIsMonotonic`              — adding XP never *decreases* total level×curve progress
  - `prop_levelUpFullHeals`               — after `levelUp`, `sHP == sMaxHP`
  - `prop_levelUpIncreasesMaxHP`          — `sMaxHP` strictly increases on level-up
  - `prop_xpCurveMonotonic`               — `xpForNextLevel` is strictly increasing
  - `prop_multiLevelUpConsumesCorrectXP`  — dumping a huge XP pile at level 1 lands on the right level with leftover XP ≤ the new threshold
  - `prop_killGrantsXP`                   — integration: player vs. a monster, `Kill` result, player XP/level updates

**Deliverable:** Killing monsters shows an XP gain message; hitting a threshold shows a level-up message, restores HP, and the status bar reflects the new level.

### Milestone 8: Music and Sound Effects

**Goal:** Looping background music plus short SFX for attack, hit, death, level-up, pickup, and stairs. Audio is IO-only and lives strictly at the edge next to rendering — the pure logic layer stays IO-free.

**Library choice:** `proteaaudio-sdl` (primary) — BSD-3, actively maintained, plays WAV/OGG/MP3, has a built-in multichannel mixer. System dep on Debian/Ubuntu is `libsdl2-dev`. Add as a stack.yaml `extra-dep`.

**Fallback:** `sdl2-mixer` if the primary hits a build issue — same SDL2 system deps, proven API, but staler.

**Avoid:** ALUT (WAV-only, upstream dead), raw PortAudio/hsndfile (no mixer, ancient).

#### Architectural change: semantic events

Up to now the turn resolution has been `GameAction -> GameState -> GameState`. To drive audio cleanly, refactor it to also emit a list of semantic events:

```haskell
data GameEvent
  = EvStep
  | EvAttackHit
  | EvAttackMiss
  | EvAttackCrit
  | EvMonsterHit
  | EvMonsterKilled MonsterKind
  | EvPlayerHurt
  | EvPlayerDied
  | EvLevelUp
  | EvPickup
  | EvStairsDown
  | EvStairsUp
  deriving (Eq, Show)

stepTurn :: GameAction -> GameState -> (GameState, [GameEvent])
```

This is the ONLY change the pure layer needs. `applyAction` and `processMonsters` collect events as they resolve actions; everything else stays pure and testable.

Modules:
- `Game/Audio.hs` — thin IO shell:
  - `data AudioSystem` — opaque handle wrapping music sample + preloaded SFX map
  - `initAudio       :: IO AudioSystem`
  - `loadDefaultBank :: AudioSystem -> IO ()`   -- loads the OGG files under `assets/`
  - `playEvent       :: AudioSystem -> GameEvent -> IO ()`
  - `shutdownAudio   :: AudioSystem -> IO ()`
- `app/Main.hs`:
  - Init audio before `customMain`, store `AudioSystem` in an `IORef` (or refactor Brick state to carry it), start music loop, tear down on exit
  - In the event handler, after applying the pure `stepTurn`, iterate the emitted `[GameEvent]` and `liftIO (playEvent audio e)` for each
- `Game/GameState.hs` — `applyAction`/`processMonsters` refactored to return `(GameState, [GameEvent])`; existing tests updated to ignore the event list (or to assert on it)

Assets (committed under `assets/`, OGG Vorbis only to sidestep any MP3 discussion):
```
assets/
├── music/
│   └── theme.ogg
└── sfx/
    ├── attack.ogg
    ├── hit.ogg
    ├── miss.ogg
    ├── crit.ogg
    ├── death.ogg
    ├── levelup.ogg
    ├── pickup.ogg
    └── stairs.ogg
```

Placeholder assets are fine to start — the `sfxr` / `jsfxr` / `bfxr` family generates perfectly usable roguelike blips and can export OGG/WAV directly.

Tests:
- The audio layer itself is IO and not unit-tested beyond "it starts and stops without crashing".
- The event list IS pure and testable. In `GameStateSpec.hs` (new):
  - `prop_killEmitsMonsterKilledEvent`   — attacking a 1-HP monster yields an `EvMonsterKilled` event
  - `prop_playerDeathEmitsDiedEvent`     — fatal damage yields `EvPlayerDied`
  - `prop_levelUpEmitsLevelUpEvent`      — gaining threshold XP emits `EvLevelUp` (once M7 is in)
  - `prop_movingOntoStairsEmitsStairsEvent`
  - `prop_pickupEmitsPickupEvent`
- Optional: a `NullAudio` backend (`playEvent = \_ _ -> pure ()`) for running the game without system audio, handy for CI and headless runs.

Gotchas:
- `stack.yaml` will need `extra-deps: [proteaaudio-sdl-0.10.1, proteaaudio-0.10.1]` (or whatever the current version is)
- System deps: `sudo apt install libsdl2-dev` on Debian/Ubuntu
- Licensing: `proteaaudio-sdl` is BSD-3; SDL2 is zlib. Ship OGG assets you have rights to. MP3 patents have expired (2017), but sticking to OGG keeps the discussion short.
- Non-blocking: proteaaudio's mixer is fire-and-forget, so firing SFX from the Brick event handler won't stall the render loop. No threading needed for a turn-based game.

**Deliverable:** Starting the game plays a music loop; attacking, getting hit, killing a monster, leveling up, and descending stairs each play a distinct SFX. Game still runs if audio init fails (fall back to `NullAudio`).

### Milestone 9: Slash Commands / Typed Command Prompt

**Goal:** Open a typed command prompt with `/` (or `:`) and dispatch parsed commands. First use case is a wizard/debug console that makes playtesting later milestones easier; the same plumbing then carries richer gameplay verbs that don't deserve a hotkey.

This slots in wherever convenient — it's genuinely independent of the content milestones (M5 items, M6 quests). Pulling it forward is attractive *because* the wizard commands will speed up work on those milestones.

**Precedent:** NetHack's `#extended` commands, Caves of Qud's slash bar, DCSS's prompts. Well-trodden ground in terminal roguelikes.

Modules:
- `Game/Types.hs` — extend `GameAction` with `CommandLine String`, OR add a parallel `data Command` type that the command parser emits and `applyAction` routes on.
- `Game/Logic/Command.hs` (new) — pure parser:
  - `data Command = CmdTeleport Pos | CmdSpawn MonsterKind | CmdHeal | CmdKillAll | CmdRevealMap | CmdDescend | CmdAscend | ...`
  - `parseCommand :: String -> Either ParseError Command`
  - Start with a simple whitespace-split + keyword match; parsec/megaparsec only if the grammar grows.
- `Game/GameState.hs` — `applyCommand :: Command -> GameState -> GameState`, called from `applyAction` when a `CommandLine` comes in. Keeps the pure layer self-contained.
- `Game/Input.hs` — add a `PromptMode` flag (or a sibling state field) so that while the prompt is open, keystrokes append to a buffer instead of triggering movement. `Enter` submits, `Esc` cancels, `Backspace` edits, `Tab` is a stretch goal for autocomplete.
- `Game/Render.hs` — draw the prompt at the bottom of the screen when active; the existing `showCursor` call (currently parked on a blank spacer line) becomes the prompt cursor.
- `app/Main.hs` — no changes beyond routing keypresses through the prompt buffer when it's open.

Wizard command starter set (makes later milestones easier to test):
- `/tp X Y`              — teleport the player
- `/spawn <kind>`        — drop a monster at the player's feet (Rat/Goblin/Orc for now)
- `/heal`                — full HP
- `/kill-all`            — clear the current level of monsters
- `/reveal-map`          — mark every tile as explored (fog lifts)
- `/xp N`                — grant N XP (pairs well with M7 for testing the level-up curve)
- `/descend` / `/ascend` — stair actions, usable before M6 ships the real staircase gameplay

Gameplay command starter set (once core systems exist):
- `/talk`, `/pray`, `/read <item>`, `/cast <spell>`, `/quaff <potion>`

Tests:
- `CommandSpec.hs`:
  - `prop_parseRoundTrips`          — for every `Command`, `parseCommand (render c) == Right c`
  - `prop_unknownCommandIsError`    — random garbage strings always return `Left`
  - `prop_applyHealRestoresHP`      — after `CmdHeal`, `sHP == sMaxHP`
  - `prop_applyTeleportMovesPlayer` — after `CmdTeleport p`, `gsPlayerPos == p` iff the tile is walkable
  - `prop_killAllClearsMonsters`    — after `CmdKillAll`, `gsMonsters == []`
- Prompt mode itself is light IO; the pure parser carries the weight.

Stretch goals (not blocking):
- Tab autocomplete against a static command list
- Command history with up/down arrows (ring buffer in state)
- Help/discoverability: `/help` lists every known command

**Deliverable:** Pressing `/` opens a prompt; typing `/heal` + Enter fully heals the player; `/spawn rat` drops a rat adjacent to the player; an unknown command prints an error message. The existing hotkey-driven input path is untouched.

### Milestone 10: NPCs, Dialogue, and Quest Giving

**Goal:** Quests are acquired from NPCs on the map through a dialogue modal, rather than auto-seeded at spawn. Adds a quest log, accept/reject flow, and an abandon action. M6 shipped the underlying `Game.Logic.Quest` state machine — this milestone is the UX layer and content plumbing on top of it.

**Why:** quests currently just appear in the panel at game start and auto-tick. The game feels more alive when the player *finds* a quest giver, chooses whether to take the job, and can track what they've signed up for.

#### Sub-milestone A: NPC entities

- `Game/Types.hs` — new `NPC` record with position, name, a glyph (probably a letter like `N` or a color-distinct `@`), and an embedded list of offered `Quest`s.
- `GameState` — add `gsNPCs :: ![NPC]`. NPCs don't move, don't take turns, don't block combat — they're just interactable scenery.
- Dungeon generator — optionally drop one NPC in a room on depths that make sense (probably depth 1 to start, so the first run always finds a quest giver).
- `Game/Render.hs` — render NPCs with a distinct attribute so they stand out from monsters.

#### Sub-milestone B: Talk action + dialogue modal

- `Game/Types.hs` — extend `GameAction` with `Talk` (or route it through `Move Dir` bumping into an NPC tile, similar to how attack works on monsters). Bumping is probably the nicer UX — no extra hotkey.
- `GameState` — `playerTalk :: NPC -> GameState -> GameState` opens a modal (same pattern as the inventory modal: a `gsDialogue :: Maybe DialogueState` field).
- `DialogueState` holds the current NPC's line, the list of quests they're offering, a cursor position, and a mode (browsing / confirming).
- `Game/Render.hs` — `drawDialogueModal` similar to `drawInventoryModal`, centered, showing the NPC name, greeting, and a lettered list of quests. Each quest entry shows its description and status (available / already accepted / already completed elsewhere).
- `app/Main.hs` — route keys through a `handleDialogueKey` branch when the modal is open. Esc closes; letters pick a quest; Enter/`y` accepts; `n` rejects.

#### Sub-milestone C: Accept / reject flow

- `Game.Logic.Quest` — add a `QuestNotAccepted` status (or repurpose `QuestNotStarted`, which was already reserved for exactly this). Accepting flips it to `QuestActive`; `advanceQuest` already ignores non-active quests so no change needed there.
- `GameState` — quests offered by an NPC live on the NPC until accepted. Accepting copies the quest into `gsQuests` with status `QuestActive` and removes it from that NPC's offer list (so you can't double-accept). Rejecting leaves it on the NPC so the player can come back later.
- Starter content: remove the hardcoded `starterQuests` from `mkGameState`; instead, spawn a "Quest Master" NPC on depth 1 with the Slayer (kill 5) and Delve (reach 3) quests pre-loaded.

#### Sub-milestone D: Quest log screen

- New modal: press `j` (journal) or `Q` to open a full-screen quest log.
- Three sections: Active, Completed, Failed.
- Each entry shows name, full description (`questDescription`), and progress label.
- `Game/Render.hs` — `drawQuestLogModal`, parallel to the inventory modal.

#### Sub-milestone E: Abandon

- From the quest log, pressing a letter + `x` (or similar) marks an active quest as `QuestFailed`. `advanceQuest` already treats `QuestFailed` as absorbing so the abandoned quest stays visible in the log but never makes further progress.
- Confirm prompt before abandoning to prevent fat-finger misclicks.

Tests:
- `NPCSpec.hs` (new) — NPC placement, bumping opens dialogue, dialogue state transitions.
- `Game.Logic.QuestSpec` — extend with accept/reject/abandon transitions on top of the existing state machine tests.
- `GameStateSpec` — integration: build a fixture with an NPC offering a kill quest, simulate talk → accept → kill → completion.

Content stretch goals (not blocking):
- Multiple NPCs per run, each with flavor text and their own quest pool.
- Quest rewards: XP bounty, gold, item drops on completion.
- Simple NPC dialogue trees (more than one line of flavor text, yes/no branches).
- A "reputation" or "known quests" tracker so accepted-then-abandoned quests can be re-offered differently.

**Deliverable:** Starting a new run, no quests are in the panel. The player finds an NPC on depth 1, bumps into them, a dialogue modal opens, they accept a quest, the quest appears in both the one-line panel and a full quest log opened with `j`. Rejected quests can be re-accepted on a return visit. Abandoning a quest marks it failed in the log.

---

### Milestone 11: Boss Encounter

**Goal:** Give the dungeon a climax. Somewhere in a randomized deep-floor band (default: depths 9-11) a boss spawns — a unique, tougher monster with its own multi-tile sprite, name, and stat block — placed in a purpose-built boss room at a randomized position within that room. The boss room has its own music track that kicks in when the player enters it. Killing the boss fires a dedicated event, awards a big XP bounty, and shows a victory screen. This is the first real "end state" the game has other than dying.

**Why:** the existing depth loop is open-ended and XP-driven, but it has no destination. A boss in the deep floors gives runs a shape — the player can *win* — and sets up later content (multiple bosses, boss-gated quests, post-boss endless mode). Randomizing the exact depth and position keeps repeat runs from feeling scripted: the player knows *roughly* when the fight is coming but can't just memorize coordinates.

#### Sub-milestone A: Boss data model

- `Game/Types.hs` — extend `MonsterKind` with a `Boss BossKind` constructor, or add a parallel `BossKind` enum (`Dragon`, `LichKing`, ...). Start with a single `Dragon`.
- Boss stat table lives next to `monsterStats` so balance stays in one place. Bosses should feel noticeably tougher than any regular monster (e.g., 5-10x HP of the strongest normal monster at that depth, higher attack, similar speed).
- **Multi-tile sprite.** Bosses occupy a rectangular footprint larger than 1×1 — the Dragon is a 2×2 block by default. The `Monster` record gains `mFootprint :: !(V2 Int)` (width, height) defaulting to `(1,1)` for normal monsters. `mPos` is the top-left corner of the footprint; the creature "occupies" every tile in `[mPos .. mPos + footprint - 1]`.
- Glyphs: each tile in the footprint renders a piece of the sprite. For a 2×2 Dragon use corner glyphs like:
  ```
  /\
  \/
  ```
  or the uppercase letter + decorations (`D>` / `<<`). All four tiles share the same color attribute (probably red or magenta) so the sprite reads as one entity.
- Collision: movement / attack / FOV logic that currently asks "is there a monster at this tile?" needs to treat *any* tile in a boss footprint as occupied by that boss. Add `monsterOccupies :: Monster -> Pos -> Bool` and route existing lookups through it.
- Pathfinding / AI: boss AI treats its top-left as canonical for movement decisions, but can't step into a position where any footprint tile would overlap a wall or another creature.

#### Sub-milestone B: Boss room generation (randomized depth + position)

- `LevelConfig` gains `lcBossDepthRange :: !(Int, Int)` — inclusive range of depths on which a boss *may* spawn. Default `(9, 11)`. On each new game the generator picks one depth from that range (uniform) and records it; that's the boss depth for that run.
- Boss depth is stored on `GameState` (e.g., `gsBossDepth :: !Int`) so the quest log and any hint text can reference it consistently, and so entering any depth can check "am I the boss floor?" without re-rolling.
- `Game.Logic.Dungeon` — when generating the chosen boss depth, carve one larger room (the boss room) and mark it as such. Regular monsters still populate the rest of the level but the boss room itself holds only the boss.
- **Randomized boss position within the room.** The generator picks a uniform random position inside the boss room that has enough clearance for the full footprint (so a 2×2 Dragon needs a 2×2 open area not touching a wall). Don't hardcode center — let it sit anywhere reasonable.
- The boss room must be reachable — i.e., the corridor network must connect to it. Easiest implementation: generate the boss room first, then run the normal room-and-corridor pass with it already in the room list.
- The boss depth has no `StairsDown`. Killing the boss is the only way the run ends in victory. Depths *past* the boss depth in the range (if the boss rolled shallow) behave normally — the player could, in theory, descend past the boss floor into deeper empty floors, but the generator should refuse to place stairs down on the boss floor regardless.

#### Sub-milestone C: Boss combat + victory state

- `GameState` — `applyAction` already handles monster kills generically; the only special case is detecting a boss kill. Add `EvBossKilled` to `GameEvent`. Audio layer hooks this for a victory sting.
- Because the boss spans multiple tiles, *any* attack into *any* of its footprint tiles should count as a hit on the boss. Route attack resolution through `monsterOccupies`.
- Add `gsVictory :: !Bool` to `GameState`, set when the boss dies. `gsDead` and `gsVictory` are mutually exclusive — both halt normal input.
- `Game.Render` — `drawVictoryModal` shown over the dungeon when `gsVictory` is true. Message: `"You have slain the Dragon! The dungeon is yours."` with stat summary (depth reached, turns taken if we track it, kills, level). Any key returns to the title / quits.
- `app/Main.hs` — route input while `gsVictory` is true to a handler that just waits for a keypress and halts.

#### Sub-milestone D: Boss music

- `Game/Audio.hs` — add a `BossTheme` track alongside the existing dungeon loop. Asset: a new looped ogg/mp3 in `assets/music/` (needs sourcing — same MIT-compatible channels as the existing audio).
- Trigger: when the player enters the boss room (or enters line-of-sight with the boss, whichever feels better in playtest), crossfade from the dungeon loop to the boss theme. When the boss dies, crossfade to a short victory sting and then silence (or a calm ambient outro).
- Implementation: `Audio.playMusic` gains a `MusicTrack` argument; the game state tracks `gsCurrentMusic :: !MusicTrack` so the event pump only issues a change when the track actually differs. This avoids restarting the track on every step inside the boss room.
- Fallback: if the boss theme asset is missing, fall back to the dungeon loop and log a warning — same graceful-degradation pattern the rest of the audio layer uses.

#### Sub-milestone E: Boss-as-quest-goal

- `Game.Logic.Quest` — new `GoalKillBoss` variant. A quest with this goal completes when `EvKilledMonster` carries a boss kind (or add a dedicated `EvKilledBoss` event fed into `advanceQuest`).
- Seed the Quest Master on depth 1 with a "Slay the Dragon" offer. The quest's flavor text should be vague about *which* deep floor the dragon is on ("rumors place it somewhere in the lowest halls") since the depth is randomized per run.

Tests:
- `Game.Logic.DungeonSpec` — generating with a boss depth range produces exactly one boss somewhere in that range; the boss is placed inside the boss room; the footprint fits entirely inside the room and doesn't overlap walls; the boss is reachable from the player spawn. Also: boss floor has no `StairsDown`.
- `Game.Logic.QuestSpec` — `GoalKillBoss` advances only on boss kills, not regular monster kills.
- `GameStateSpec` — integration fixture: spawn player adjacent to a 2×2 boss, attack into each of the four footprint tiles in turn, confirm each resolves as a hit on the boss; whale on it until dead, confirm `gsVictory` flips true and `EvBossKilled` is emitted.
- Multi-tile collision: the player cannot move into *any* tile of a boss footprint; monsters cannot either.

Content stretch goals:
- Boss AI: movement pattern, special attack (breath weapon hitting a line/cone), telegraph turn before the special.
- Loot drop: a guaranteed unique weapon or armor on the boss corpse.
- Multiple bosses on a rotation (Dragon / Lich / Demon) so runs vary. Each has its own footprint shape — Dragon 2×2, Lich 1×1 but with minions, Demon 2×1 wide.
- Post-boss "endless" mode: descend past the boss into increasingly brutal levels, no more bosses but scaling monsters.
- Per-boss music: different themes per `BossKind` instead of one shared `BossTheme`.

**Deliverable:** Starting a run, the player can see in their quest log "Slay the Dragon". Descending through the dungeon, somewhere between depths 9 and 11 (different each run) a larger room contains a 2×2 Dragon sprite, and entering that room crossfades the soundtrack to the boss theme. Attacking any of the four footprint tiles damages the boss; killing it shows a victory modal, fires `EvBossKilled`, ends the music, and advances the matching quest.

---

### Milestone 12: Quest Turn-In and XP Rewards

**Goal:** Quest completion becomes a two-step flow: hitting the goal condition flips the quest to a new "ready to turn in" state, and the player has to return to an NPC to actually collect the reward. Rewards are XP bounties attached to each quest, awarded when the quest is handed in. The player may turn in at the originating NPC *or* at any NPC (design decision captured below).

**Why:** right now a quest just silently ticks to `QuestCompleted` and nothing happens — there's no reward, no moment of completion, no reason to ever talk to an NPC twice. Adding a turn-in step makes NPCs into meaningful destinations, rewards exploration (backtracking feels purposeful), and gives quests actual mechanical weight via XP.

#### Design decision: any NPC vs. quest giver?

Two options, with tradeoffs:

1. **Quest giver only.** Lore-coherent ("the Quest Master wants to hear how it went"), forces the player back to the quest hub, creates natural hub-and-spoke flow. Downside: punishing on deep runs where backtracking is expensive.
2. **Any NPC.** Friendlier, rewards finding new NPCs on deeper levels (turn in at a deep NPC for convenience), lets us place NPCs sparsely without feeling like dead ends. Downside: weakens the identity of individual quest givers.

**Proposed:** support both. Every quest has a `qGiver :: Maybe NPCId` (giver identity). Turning in at the original giver awards the full XP bounty; turning in at any other NPC awards a reduced bounty (say 50%). This keeps the hub meaningful without punishing the player for exploring.

#### Sub-milestone A: Ready-to-turn-in state

- `Game.Logic.Quest` — extend `QuestStatus` with `QuestReadyToTurnIn`. Rewrite `advanceQuest` so reaching the goal flips to `QuestReadyToTurnIn` instead of `QuestCompleted`; `QuestCompleted` now strictly means "turned in and reward collected".
- `questProgressLabel` shows `"ready!"` for ready-to-turn-in quests so the panel actually draws attention.
- `Game.Render.drawQuests` — highlight ready quests with a distinct color attribute (maybe green) so the player notices without opening the log.
- Migration: existing tests that assert `QuestCompleted` after advancing past the goal need to be updated to assert `QuestReadyToTurnIn`, then a follow-up turn-in step asserts `QuestCompleted`.

#### Sub-milestone B: Reward model

- `Game.Logic.Quest` — `qReward :: !Int` (XP). `mkQuest` takes an extra argument, or a builder pattern keeps the existing 2-arg form and lets callers set `qReward` after.
- Default bounties scaled to goal difficulty: Slayer-5 gives e.g. 50 XP, Delve-3 gives 75 XP, Kill-the-Dragon gives 500.
- `qGiver :: !(Maybe Int)` — the index of the NPC that offered this quest in `gsNPCs`, captured at accept time. Stored as index for now; if NPCs ever get stable IDs separate from their list position this becomes an `NPCId`.

#### Sub-milestone C: Turn-in flow

- `GameState` — new function `turnInQuest :: Int -> Int -> GameState -> GameState` (npcIdx, questIdx). Preconditions: quest is `QuestReadyToTurnIn`; NPC exists.
- Award XP via the existing progression pipeline (`awardXp` or whatever M7 named it) so level-ups fire naturally through the shared code path — turning in can trigger `EvLevelUp`.
- If the NPC is the quest's original giver, award `qReward` in full. Otherwise award `qReward / 2` (integer division is fine). Emit a message either way: `"Quest complete! +50 XP."` or `"Quest complete! +25 XP (partial reward — not the original giver)."`.
- Flip the quest to `QuestCompleted`.
- Fire a new `EvQuestTurnedIn` game event so audio can play a reward jingle.

#### Sub-milestone D: Dialogue UX

- `drawDialogueModal` — when an NPC is visited, show a new top section: "Quests ready to turn in" listing each ready quest the player is carrying, lettered `A`..`Z` (capital letters to keep them visually separate from the quest *offers* which stay lowercase `a`..`z`).
- `handleDialogueKey` — capital letters trigger `turnInQuest`. After turn-in the modal should either stay open (so the player can chain turn-ins and then accept new quests) or close if there's nothing left on offer.
- If the player is at the quest's original giver, the ready-quest entry marks it with a `★` or similar so they know they'll get the full bounty.

Tests:
- `Game.Logic.QuestSpec` — a quest whose goal is met transitions to `QuestReadyToTurnIn`, not `QuestCompleted`; further kills beyond the target don't advance the counter. Absorbing check for `QuestReadyToTurnIn` too (it shouldn't go backwards or sideways).
- `GameStateSpec` — fixture: accept a kill-1 quest, kill a rat, verify quest is ReadyToTurnIn and player XP is unchanged. Then turn in at the original giver and verify quest is Completed and XP was awarded. Repeat with a second NPC to verify the partial bounty.
- Level-up integration: turn in a quest whose XP bounty crosses the level threshold and verify `EvLevelUp` fires.

Content stretch goals:
- Non-XP rewards: gold (once gold exists), items (a potion, a better weapon), stat boosts.
- Quest chains: turning in quest A unlocks quest B from the same NPC.
- Timed quests that auto-fail if not turned in within N turns of being ready.
- Reputation with individual NPCs so repeatedly turning in at the right giver unlocks better offers.

**Deliverable:** Accept "Slayer" from the Quest Master on depth 1. Kill 5 rats — the quest flips to `"ready!"` in the panel and glows. Ignore it and descend to depth 3, find a second NPC, bump them — the dialogue modal shows the ready Slayer quest with a `-½` marker. Turn it in at the second NPC for half XP, or trek back to the Quest Master for full XP. Turning in fires a level-up if the bounty pushes the player over the threshold.

---

### Milestone 13: Save, Quicksave, and Load

**Goal:** Make runs persistent across process restarts. The player can save the current game to disk, quicksave with a single hotkey, load any previous save from an in-game menu, or resume the most recent save from a launch-screen menu. Save files are stored in a compact **binary** format — not human-readable text — so casual editing can't trivially rewrite HP, gold, or quest state.

**Why:** the game already has enough depth (quests, leveling, inventory, a boss fight) that losing a run to an accidental `Ctrl-C`, a terminal close, or wanting to quit for dinner is genuinely painful. A save system turns the roguelike from a single-sitting experience into something the player can dip into. Binary specifically — not JSON, not YAML, not a pretty-printed `Show` dump — because a text format invites "I'll just bump my HP to 999" and that undercuts the whole risk/reward loop. Binary isn't cryptographically tamper-proof (anyone with `ghci` and the cabal file can decode it), but it raises the bar from *trivial* to *inconvenient*, which is the right tradeoff for a hobby roguelike: we're not fighting a determined adversary, we're making casual cheating annoying enough that most players won't bother.

#### Design decision: save format and library

Candidates, with tradeoffs:

1. **`binary` package (Data.Binary).** Boring, well-tested, ships as a boot library, derives from `Generic`, handles laziness cleanly. Format is not self-describing but is stable across GHC versions. **Proposed default.**
2. **`cereal`.** Strict variant of `binary`. Slightly better error messages, no lazy bytestrings surprises. Fine alternative if `binary`'s laziness bites.
3. **`store`.** Fastest, but trades portability for speed (format depends on architecture word size) and is a heavier dep. Overkill for a turn-based roguelike where save size is tiny.
4. **`aeson` JSON.** Rejected: human-readable text is exactly what we're avoiding.
5. **Hand-rolled `Show`/`Read`.** Rejected: same problem as JSON plus it breaks silently on any type change.

**Proposed:** `binary` with `DeriveGeneric` + `GHC.Generics` instances. Every type in `Game.Types`, `Game.GameState`, `Game.Logic.Dungeon`, `Game.Logic.Quest`, `Game.Logic.Inventory`, etc. gains a `Generic` deriving + an empty `instance Binary T`. `StdGen` needs a manual instance (serialize its `show` representation — `StdGen` is `Read`/`Show` but not `Binary`, and we want to preserve the exact RNG state so load-then-play is deterministic).

#### Design decision: save file location

- **Linux/macOS:** `$XDG_DATA_HOME/dungeon-haskell/saves/` (falling back to `~/.local/share/dungeon-haskell/saves/` if `XDG_DATA_HOME` is unset). Use the `directories` package (`getXdgDirectory XdgData "dungeon-haskell"`) so we don't reinvent XDG resolution.
- **Windows:** `%APPDATA%\dungeon-haskell\saves\` — `directories` handles this automatically.
- Filenames: `slot-<N>.save` for numbered manual slots, `quicksave.save` for the single quicksave slot. Extension is `.save` (not `.bin`) so the player can recognize them at a glance.
- The saves directory is created on demand on first write; load gracefully reports "no saves" if the directory is absent.

#### Design decision: format versioning

Binary-encoded Haskell records are brittle across type changes — add a field, every old save breaks. To avoid silently corrupting loads after schema changes:

- Every save file begins with a fixed **magic header**: the 8 bytes `"DHSAVE01"` (`D`ungeon `H`askell `SAVE`, format version `01`). On load, refuse anything without the header or with a different version.
- On a schema bump, increment the version (`"DHSAVE02"`, ...). Old saves are rejected with a clear message: `"Save file is from an older version of the game and can't be loaded."` No migration path in v1 — we're a hobby project, not a live service.
- The magic also doubles as a trivial integrity check: random file corruption usually trashes the header, so we refuse to decode garbage.

#### Sub-milestone A: Binary instances for all game types

- Add `binary` to the library's `build-depends` in the cabal file.
- Every data type that transitively appears in `GameState` gets `deriving (Generic)` and an empty `instance Binary T`. This includes (non-exhaustive): `Stats`, `Pos`/`V2 Int`, `Tile`, `DungeonLevel`, `Room`, `Monster`, `MonsterKind`, `BossKind`, `Item`, `ItemKind`, `Inventory`, `Quest`, `QuestGoal`, `QuestStatus`, `QuestEvent`, `NPC`, `GameEvent`, `GameState`, `ParkedLevel`.
- `V2 Int` from `linear` — the package already provides a `Binary` instance behind a flag or needs a manual `instance Binary (V2 Int) where put (V2 x y) = put x *> put y; get = V2 <$> get <*> get`. Check `linear`'s cabal features first; if not, write the manual instance in a new `Game.Save` module as an orphan (allowed project-wide — `-Wno-orphans` is already set).
- `StdGen`: manual instance.
  ```haskell
  instance Binary StdGen where
    put g = put (show g)
    get   = read <$> get
  ```
  The `show`/`read` round-trip for `StdGen` is stable across `random` minor versions and preserves the exact RNG stream. Accept the minor format waste (tens of bytes) for the portability win.
- `Data.Vector` and `Data.Set`: `binary` already has instances for `Vector` (via the `vector-binary-instances` package, add as dep) and `Set` (built in).
- No `Binary` instance on the audio layer — save files must never reference `PA.Sound`/`PA.Sample` handles, which are IO-side only. The `AudioSystem` lives in `Main.hs`'s closure, not in `GameState`, so this is already a non-issue; confirm by grepping for any audio type in `Game.GameState` before committing.

#### Sub-milestone B: The `Game.Save` module

New module `src/Game/Save.hs` owning every save/load operation. Pure helpers + thin IO shell:

```haskell
module Game.Save
  ( SaveError(..)
  , SaveSlot(..)
  , saveMagic
  , encodeSave        -- pure: GameState -> ByteString
  , decodeSave        -- pure: ByteString -> Either SaveError GameState
  , saveDir           -- IO FilePath (XDG-resolved, created on demand)
  , slotPath          -- SaveSlot -> IO FilePath
  , writeSave         -- SaveSlot -> GameState -> IO (Either SaveError ())
  , readSave          -- SaveSlot -> IO (Either SaveError GameState)
  , listSaves         -- IO [(SaveSlot, SaveMetadata)]
  , deleteSave        -- SaveSlot -> IO ()
  ) where

data SaveSlot = QuickSlot | NumberedSlot !Int
  deriving (Eq, Ord, Show)

data SaveError
  = SaveMissing        -- no file at that slot
  | SaveWrongMagic     -- header doesn't match DHSAVE
  | SaveWrongVersion   -- magic matches but version doesn't
  | SaveCorrupt String -- binary decoder raised an error
  | SaveIOError String -- disk IO blew up
  deriving (Eq, Show)

data SaveMetadata = SaveMetadata
  { smSlot      :: !SaveSlot
  , smDepth     :: !Int
  , smPlayerLvl :: !Int
  , smTurns     :: !Int       -- if/when we track turns
  , smTimestamp :: !UTCTime   -- file mtime, for the menu sort order
  }
```

- `encodeSave gs = saveMagic <> runPut (put gs)` — the magic is prepended as raw bytes, *not* via `put`, so the header is always readable even if the decoder chokes.
- `decodeSave bs` strips the first 8 bytes, checks the magic and version, then runs `runGetOrFail (get :: Get GameState)` on the remainder. Any `Left` or leftover bytes becomes `SaveCorrupt`.
- `writeSave` writes atomically: encode to a tempfile `slot-1.save.tmp`, `hFlush`, `renameFile` over the target. A crash mid-write can't corrupt an existing save.
- `listSaves` scans `saveDir`, parses filenames into `SaveSlot`, reads the header + first few fields of each to build the metadata list without decoding the whole state. Sort by mtime descending so the menu shows the most recent save first.
- All IO is wrapped in `try` and funneled into `SaveError` — the save system, like the audio system, must never crash the game on a missing file or permission error.

#### Sub-milestone C: Quicksave and quickload

Simplest slice of the feature — single slot, single key each way, no menu:

- Keybinds: `F5` quicksave, `F9` quickload. These are the classic Valve-era defaults, and neither currently has a meaning in the game.
- Quicksave: writes the current `GameState` to `QuickSlot`. Shows a message line: `"Quicksaved."` on success, `"Quicksave failed: <error>"` on failure. Quicksave is a free action — it does not advance monsters.
- Quickload: reads `QuickSlot`, replaces the entire `GameState` with the loaded value. On failure, shows `"No quicksave to load."` and leaves the current state untouched. Quickload does advance the world by zero turns — the loaded state is whatever was saved.
- Edge case: quickloading mid-dialogue / mid-prompt / mid-modal. The loaded state restores whatever modal flags were in the saved state, so closing modals before saving is the player's responsibility. (Alternative: force-clear all modal flags on save. Reject this — breaks "save is a snapshot" as a mental model.)
- The `updateMusicFor` call in `Main.hs` runs after every key event anyway, so quickloading from the dungeon floor back into the boss room auto-crossfades to the boss theme — the audio shell reacts to the new state on the next frame without any save-specific hook.

#### Sub-milestone D: Save menu and load menu

Two new modals, implemented as a single `SaveMenuMode` variant on `GameState`:

- `gsSaveMenu :: !(Maybe SaveMenu)` where `data SaveMenu = SaveMenu { smMode :: !SaveMenuMode, smSlots :: ![SaveSlotEntry], smCursor :: !Int }` and `data SaveMenuMode = SaveMode | LoadMode`.
- Opening: slash command `/save` opens the save menu; `/load` opens the load menu. Also bound to `F2` (save menu) and `F3` (load menu) for mouse-free access. Opening the menu triggers `listSaves` and stores the result in the menu state — the modal is a snapshot, not a live view.
- Render: list slots as `a` `Quick  — depth 7  lvl 4  · 2h ago`, `b` `Slot 1 — depth 11 lvl 8  · yesterday`, etc. Highlight the cursor row. Empty slots in SaveMode render as `<empty>`; empty slots in LoadMode are greyed out / unselectable.
- Input (save mode): `a`..`z` = pick slot to overwrite (with a confirm prompt if the slot is non-empty — reuse the `gsConfirmQuit` pattern), `Esc` / `F2` = close.
- Input (load mode): `a`..`z` = pick slot to load, `x` = delete the selected save (with confirm), `Esc` / `F3` = close.
- After a successful save/load the menu closes and a status message reports the outcome.
- Handler priority in `app/Main.hs`: the save menu slots in between the quest log and the inventory in the modal chain, so the player can't open it while the quit confirm is up.

#### Sub-milestone E: Launch-screen menu

Right now the game boots straight into `newGame`. With saves, the player should get a chance to continue an existing run instead of being forced into a fresh one.

- New module `src/Game/Menu.hs` (or extend `Main.hs` if keeping it thin) that draws a minimal title-screen widget with three options:
  1. **New Game** — same as today's `newGame gen defaultLevelConfig`.
  2. **Continue** — loads the most recent save (by mtime), whatever slot. Disabled if no saves exist.
  3. **Load Game** — opens the load menu directly against the save directory, player picks a slot.
  4. **Quit** — halts without launching the game loop.
- The launch menu is a separate `Brick` app from the main game app, or (simpler) a `GameState` flag like `gsLaunchMenu :: !Bool` that the main app respects: when true, the renderer draws the title screen and the event handler routes keys to the menu. On "New Game" / "Continue" / "Load Game" the flag flips false and the real game takes over.
- This is also the natural place for the game title, a version string, and eventually a credits link.

#### Sub-milestone F: Wiring into the event pump

- `Game.Input` — new `GameAction` variants: `Quicksave`, `Quickload`, `OpenSaveMenu`, `OpenLoadMenu`. The F-keys are not currently in the keymap; add them.
- `GameState.applyAction` — does *not* handle save/load directly. Save/load are IO (they talk to the filesystem) so they stay in `Main.hs`'s event handler, same way audio does. Add a new handler branch: `Just Quicksave -> liftIO (Save.writeSave Save.QuickSlot gs) >>= reportSaveResult`.
- Save and load are free actions: they don't advance monster AI. The handler skips the `applyAction` call entirely for these variants. Tests will pin this down.
- `reportSaveResult :: Either SaveError () -> EventM () GameState ()` appends a user-facing line to `gsMessages`.

Tests:
- `Game.SaveSpec` (new) — round-trip property: `forAll gameState $ \gs -> decodeSave (encodeSave gs) == Right gs`. Requires `Arbitrary GameState` which is nontrivial; start with a hand-built fixture generator that uses `newGame` with random seeds and then runs a few random `applyAction` steps, rather than a true `Arbitrary` instance.
- Round-trip preserves RNG determinism: save mid-run, load, run 20 more random actions from the loaded state, compare against a parallel universe that didn't save — they must be identical.
- Magic-header rejection: `decodeSave (BS.drop 1 (encodeSave gs))` returns `Left SaveWrongMagic`. `decodeSave (BS.pack "DHSAVE99" <> rest)` returns `Left SaveWrongVersion`.
- Corruption rejection: flipping a random byte past the header produces `Left (SaveCorrupt _)` — not a crash.
- `writeSave` / `readSave` integration test using `withSystemTempDirectory` so the test suite doesn't touch the real XDG data dir.
- `GameStateSpec` — quicksave during an open inventory modal, quickload, verify the modal is still open (snapshot semantics).

Content stretch goals:
- **Autosave** on descend-stairs or on level-up, written to a dedicated `autosave.save` slot that's never directly writable by the player. Gives a recent-enough restore point without cluttering the save menu.
- **Permadeath mode** as a new-game option: the save file is deleted on player death. Classic roguelike flavor.
- **Save compression** via `zlib` (`Codec.Compression.GZip.compress` around the binary payload) if saves ever grow large. Dungeons with explored sets will compress well; not worth the complexity until we see multi-megabyte saves.
- **Save metadata screenshot**: render a tiny ASCII thumbnail of the current level (or just the explored portion around the player) and include it in the save metadata so the load menu shows a visual preview.
- **Checksum** (CRC32 or Blake2 truncated) after the payload so the game can distinguish "corrupted save" from "save from a broken build".
- **Cloud save sync** via Syncthing-friendly file layout — already the case if the save dir is under XDG data, so technically free.

**Deliverable:** Start a run, descend to depth 4, open the inventory, quaff a potion, then press `F5` — a `"Quicksaved."` message appears. Close the terminal entirely. Relaunch `dungeon-haskell`: the title screen shows New Game / Continue / Load / Quit with Continue highlighted as the default. Picking Continue drops the player back on depth 4 with the exact HP, inventory, quest progress, and RNG state from the save. Running the same action sequence from both the original run and the loaded run produces identical results. Opening `slot-1.save` in a hex editor shows `DHSAVE01` followed by opaque binary — no plaintext HP values to tweak.

---

### Milestone 14: AI-Powered Content Generation

**Goal:** Integrate LLM-based content generation into the dungeon crawler
to dynamically produce quests, level flavor, and NPC behaviour. All LLM calls
go through **http-tower-hs** which provides resilient HTTP middleware (retries,
timeouts, circuit-breaking) so the game degrades gracefully when the AI backend
is unavailable.

The guiding principle is **AI-optional**: every feature must have a sensible
hardcoded fallback so the game never blocks on a network call.

#### Configuration

All game configuration lives in a `config.yaml` file, loaded at startup via
the `yaml` (Data.Yaml) package. The game searches for the file in this order:

1. Path passed via `--config` CLI flag (if provided)
2. `./config.yaml` (current working directory)
3. `$XDG_CONFIG_HOME/dungeon-haskell/config.yaml`

If no config file is found, the game starts with sensible defaults (AI
features disabled, all other settings at their default values).

```yaml
# config.yaml
ai:
  enabled: true
  endpoint: "http://localhost:11434/api/generate"   # Ollama default
  api_key: ""                                        # empty = no auth header
  model: "mistral"
  timeout_seconds: 15
  max_retries: 3

audio:
  music_volume: 0.7
  sfx_volume: 1.0
```

```haskell
-- src/Game/Config.hs
data GameConfig = GameConfig
  { gcAI    :: !AIConfig
  , gcAudio :: !AudioConfig
  } deriving (Eq, Show, Generic)

data AIConfig = AIConfig
  { aiEnabled  :: !Bool
  , aiEndpoint :: !Text
  , aiApiKey   :: !Text
  , aiModel    :: !Text
  , aiTimeout  :: !Int      -- seconds
  , aiRetries  :: !Int
  } deriving (Eq, Show, Generic)

instance FromJSON GameConfig
instance FromJSON AIConfig

loadConfig :: Maybe FilePath -> IO GameConfig
-- tries CLI path, then ./config.yaml, then XDG, then returns defaults
```

Ship a `config.yaml.example` in the repo root documenting all options.

---

#### Step 1 — Add http-tower-hs, YAML & JSON dependencies

| Task | Detail |
|------|--------|
| Add `http-tower-hs` to `dungeon-haskell.cabal` dependencies | Use the published Hackage version (`http-tower-hs ^>= 0.3.1`); add to `stack.yaml` `extra-deps` only if not yet on the current Stackage resolver |
| Add library deps to `dungeon-haskell.cabal` | `http-tower-hs`, `aeson`, `yaml`, `text`, `http-client`, `http-client-tls`, `bytestring` |
| Create `src/Game/Config.hs` | `GameConfig` / `AIConfig` types with `FromJSON` instances; `loadConfig` function (see above) |
| Create `src/Game/AI/Client.hs` | Thin wrapper that configures http-tower-hs with retry policy (3 attempts, exponential backoff), 5 s connect / 15 s response timeout, and a circuit breaker (5 failures → 30 s open) |
| Wire config into startup | `Main.hs` calls `loadConfig`, stores result in a top-level `AppEnv` passed through `IO` |

**Acceptance:** `stack build` succeeds; a manual `cabal repl` call to the client
module can round-trip a prompt to a local Ollama or OpenAI-compatible endpoint.

---

#### Step 2 — Dynamic Quest Generation

##### 2.1 Prompt design

Create `src/Game/AI/Prompts.hs` with a quest-generation prompt template:

```
You are a quest designer for a dungeon crawler.
The player is on depth {depth}, level {level}, has killed {kills} monsters.
Generate a quest as JSON:
{
  "name": "<short title>",
  "description": "<1-2 sentences>",
  "goal": "kill <N> <monster>" | "reach depth <D>" | "kill boss",
  "xp_reward": <int>
}
Respond with ONLY the JSON object.
```

##### 2.2 Quest parsing

In `src/Game/AI/QuestGen.hs`:

- `generateQuest :: AIConfig -> GameState -> IO (Maybe Quest)`
- Parse the JSON response into the existing `Quest` / `QuestGoal` types.
- On parse failure or timeout → return `Nothing` (caller falls back to
  hardcoded quest pool).

##### 2.3 Integration point

In `Game.Logic.Dungeon.generateLevel` (or the NPC-placement path):

- When placing the Quest Master NPC on depth 1, attempt to generate 1-2
  AI quests alongside the existing 3 hardcoded ones.
- On deeper floors with NPCs, generate floor-appropriate quests.

**Acceptance:** Talking to the Quest Master shows at least one AI-generated
quest when the LLM endpoint is reachable; shows only hardcoded quests when
it is not.

---

#### Step 3 — NPC Greeting Behaviour

##### 3.1 Prompt design

Add to `Prompts.hs`:

```
You are an NPC in a dark dungeon. Your name is {name}. You are a {role}.
The adventurer approaches you on dungeon depth {depth}.
Greet them in 1-2 short sentences. Be atmospheric and in-character.
Respond with ONLY the greeting text, no quotes.
```

##### 3.2 Greeting generation

In `src/Game/AI/NPCBehaviour.hs`:

- `generateGreeting :: AIConfig -> NPC -> Int -> IO (Maybe Text)`
  (NPC, current depth → optional greeting)
- Cache the generated greeting in the NPC record so we only call the LLM
  once per NPC per game session.
- Fallback: use the existing hardcoded `npcGreeting` field.

##### 3.3 NPC record changes

Extend `NPC` in `Game.Types`:

```haskell
data NPC = NPC
  { npcName      :: Text
  , npcPos       :: Pos
  , npcGreeting  :: String          -- hardcoded fallback
  , npcAIGreet   :: Maybe String    -- cached AI greeting (Nothing = not yet fetched)
  , npcRole      :: NPCRole         -- new: Merchant | QuestGiver | Hermit | ...
  , npcQuests    :: [Quest]
  }
```

Display logic: prefer `npcAIGreet` when `Just`, else `npcGreeting`.

**Acceptance:** An NPC greets the player with a unique AI-generated line that
varies between game sessions. Falls back to the static greeting when offline.

---

#### Step 4 — Level Content / Room Descriptions

##### 4.1 Prompt design

```
Describe a dungeon room in 1 short atmospheric sentence.
Room size: {w}x{h}. Depth: {depth}. Monsters present: {monsters}.
Respond with ONLY the description.
```

##### 4.2 Room description generation

In `src/Game/AI/LevelContent.hs`:

- `describeRoom :: AIConfig -> Room -> Int -> [MonsterKind] -> IO (Maybe Text)`
- Generate descriptions for a batch of rooms on level entry (fire requests
  concurrently via `async` if http-tower-hs supports it, otherwise
  sequentially).
- Store in a `Map RoomId Text` on `GameState`.

##### 4.3 Display

Show the room description in the message log when the player first enters a
room (track visited rooms per level in a `Set RoomId`).

**Acceptance:** Entering a new room prints a flavour sentence.
Without LLM access, no description is shown (silent fallback).

---

#### Step 5 — Async / Non-Blocking Architecture

The Brick event loop is single-threaded. LLM calls must not freeze the UI.

| Approach | Detail |
|----------|--------|
| Background thread | On level generation or NPC interaction, fork an `IO` thread that writes the result into a `TVar` / `TChan` |
| Brick custom event | Use `BChan` to push an `AIResponseEvent` back into the Brick event loop when the result arrives |
| Placeholder text | While waiting, show "..." or the hardcoded fallback; replace with AI text when the event fires |

New module: `src/Game/AI/Async.hs`

- `requestAI :: BChan AppEvent -> AIConfig -> AIRequest -> IO ()`
  Forks a thread, makes the http-tower-hs call, pushes result onto BChan.
- `data AIRequest = GenQuest ... | GenGreeting ... | GenRoomDesc ...`
- `data AIResponse = QuestResult ... | GreetingResult ... | RoomDescResult ...`

Add `AIResponseEvent AIResponse` to the app event type in `Main.hs`.

**Acceptance:** The game never freezes during LLM calls. A slow/dead endpoint
results in the fallback content appearing with no visible delay.

---

#### Step 6 — Testing

| Task | Detail |
|------|--------|
| `config.yaml.example` | Ship in repo root documenting all config options with comments |
| Ollama instructions | Add a section in README for running a local model (e.g. `ollama run mistral`) for free local testing |
| Mock client | `src/Game/AI/Mock.hs` — returns canned responses; used when no endpoint is configured or in tests |
| Unit tests | Parse-round-trip tests for quest JSON, greeting trimming, room description length limits |
| Config tests | Verify `loadConfig` parses example file correctly; verify defaults when no file exists |
| Integration test | Optional test that hits a real endpoint (gated behind a config flag) |

---

#### New Module Dependency Graph

```
Game.Config           -- config.yaml parsing, GameConfig / AIConfig types
Game.AI.Client        -- http-tower-hs configuration, raw sendPrompt
Game.AI.Prompts       -- prompt templates
Game.AI.QuestGen      -- quest generation + JSON parsing
Game.AI.NPCBehaviour  -- greeting generation
Game.AI.LevelContent  -- room descriptions
Game.AI.Async         -- non-blocking Brick integration
Game.AI.Mock          -- canned responses for testing / offline
```

All modules under `Game.AI.*` depend on `Game.AI.Client` and `Game.Config`.
`Game.AI.Async` depends on all the others and is the single integration
surface used by `Main.hs` and `GameState.hs`.

---

#### Suggested Implementation Order

1. **Config + Client** (step 1) — get config.yaml loading and a working LLM call from Haskell
2. **NPC Greetings** (step 3) — smallest surface area, proves the pipeline
3. **Quest Generation** (step 2) — higher value, builds on the client
4. **Async wrapper** (step 5) — make it non-blocking before adding more calls
5. **Room Descriptions** (step 4) — nice-to-have layer on top
6. **Testing & polish** (step 6) — mock client, tests, README

---

#### Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| http-tower-hs Hackage version lags or has breaking changes | Pin version bounds in cabal file; worst case, wrap `http-client` + `retry` manually |
| LLM returns unparseable output | Strict JSON schema in prompt; fallback to hardcoded content on any parse error |
| Latency ruins game feel | Async architecture (step 5); pre-generate content during level loading |
| Cost of API calls | Support local Ollama; cache aggressively; generate in batches |
| Save/load breaks with new fields | New fields use `Maybe` with default `Nothing` for backward compat in `Binary` instance |

---

### Milestone 15: Doors, Keys, Treasure Rooms, and Traps

**Goal:** Turn the BSP-generated dungeons into properly explorable spaces.
Rooms become discrete spaces separated by doors; some doors are locked
and require keys found elsewhere on the floor; a marked treasure room
per level rewards the detour with denser loot; and hidden traps scattered
on floor tiles punish careless movement. The `Door` tile already exists
in `Game.Types` and is wired through walkability, FOV, rendering, and the
save codec — the generator just never emits it. This milestone closes
that gap and builds gameplay on top of it.

The guiding principle is **generator-first**: doors, keys, treasure, and
traps are all placed deterministically from the level's `StdGen` during
`generateLevel`, so the entire level state still round-trips through the
save codec and there is no hidden runtime state.

---

#### Step 1 — Place doors in the BSP generator

Today `Game.Logic.Dungeon` carves rooms and connects them with L-shaped
corridors but only ever emits `Floor` / `Wall` / `StairsDown` / `StairsUp`.
Wire the existing `Door` tile into the generator.

| Task | Detail |
|------|--------|
| Identify door sites | After corridors are stamped, walk each room's perimeter and find tiles where a corridor floor meets the room edge. Those junction tiles become door candidates |
| Roll door state | ~70% `Door Open`, ~30% `Door Closed` (Step 2 adds `Locked`); use the level's `StdGen` so placement is deterministic |
| Skip degenerate rooms | Tiny rooms (< 3x3) and the room containing spawn or stairs-up get no closed doors so the player is never softlocked on arrival |
| Make doors walk-through-once | Bumping into a `Door Closed` turns it into `Door Open` (no key required); movement cost is the same as walking |

Existing infrastructure that does **not** need to change:
- `isWalkable (Door Open/Closed)` in `Game.Types` — already correct
- `transparent` in `Game.Logic.FOV` — already returns `False` for `Closed`
- `Game.Render` — already draws `/` and `+`
- `Binary DoorState` in `Game.Save` — already derived

**Acceptance:** A generated level contains visible doors (`/` and `+`)
where corridors meet rooms; the player can bump a `+` to turn it into a
`/`; closed doors block FOV until opened; the level round-trips through
`encodeSave`/`decodeSave` unchanged.

---

#### Step 2 — Locked doors and keys

Extend `DoorState` so some doors need a matching key:

```haskell
-- Game.Types
data DoorState = Open | Closed | Locked !KeyId
  deriving (Eq, Show)

newtype KeyId = KeyId Int
  deriving (Eq, Ord, Show)
```

Add a new item kind for keys:

```haskell
data Item
  = IPotion !Potion
  | IWeapon !Weapon
  | IArmor  !Armor
  | IKey    !KeyId
```

| Task | Detail |
|------|--------|
| `isWalkable (Door (Locked _))` → `False` | Existing `canWalk` in `Movement` already gates on `isWalkable`; no caller changes needed |
| Bump-to-open for locked doors | `Game.Logic.Movement` (or the `handleBump` site in `GameState`) checks the inventory for a matching `IKey` when the bumped tile is `Door (Locked k)`; on match, the key is consumed and the door becomes `Door Open`; on miss, emit an `EvDoorLocked` event and keep the player in place |
| Key placement | The generator picks a floor tile inside a *different* room from the locked door and stamps the matching `IKey` onto `dlItems` (or whatever field carries floor items) |
| Rendering | New glyph for `Locked` (e.g. `&` in yellow); keys render as `k` |
| Messages | `"The door is locked."` / `"You unlock the door with the iron key."` via the existing message log |
| Save codec | `KeyId` gets a `Binary` instance alongside the other `DoorState` variants; existing codec version bumps to `DHSAVE04` |

**Acceptance:** A locked door blocks movement and FOV; dropping the
matching key into the inventory and bumping the door opens it and
consumes the key; a save made on a level with locked doors round-trips.

---

#### Step 3 — Treasure rooms

One room per level is designated a **treasure room**: it's locked behind
a `Door (Locked k)` and stocked with 2–4 extra items from a higher-tier
loot pool than the floor normally rolls. The matching key is placed
somewhere on the same level.

| Task | Detail |
|------|--------|
| Select treasure room | During `generateLevel`, after room placement but before door placement, pick one room: prefer small rooms far from the stairs-up tile (Chebyshev distance) so it feels like a detour, not a trivial side-step |
| Lock its door | Whichever doorway connects the treasure room to the rest of the map is forced to `Locked k` for a freshly-minted `KeyId` |
| Stock with loot | Roll 2–4 items from `Logic.Loot` at one tier above the normal depth (e.g. depth 3 rolls from the depth 4 table); guarantee at least one weapon or armor, not just potions |
| Record which room is the treasure room | Add `dlTreasureRoom :: !(Maybe Int)` (index into `dlRooms`) to `DungeonLevel` so M14's room-description hook can tag it and so the boss floor can opt out |
| Boss floor skip | Depth 10 has its own fixed geometry; no treasure room is added there |

**Acceptance:** On a fresh depth 1 floor, exactly one room is behind a
locked door and contains a dense item cluster; the key is findable
elsewhere on the same floor (not behind the same door it unlocks);
`listSaves` metadata still reports the correct depth.

---

#### Step 4 — Traps

Hidden floor hazards that trigger when the player (not a monster) steps
onto them.

```haskell
-- Game.Types
data TrapKind = Spikes | Dart | Alarm
  deriving (Eq, Show, Enum, Bounded)

data Trap = Trap
  { tPos    :: !Pos
  , tKind   :: !TrapKind
  , tHidden :: !Bool   -- True until triggered or revealed
  } deriving (Eq, Show)
```

Add `dlTraps :: ![Trap]` to `DungeonLevel`.

| Kind | Effect |
|------|--------|
| `Spikes` | 1d4 damage, emits `EvPlayerHurt`, becomes visible |
| `Dart` | 1d3 damage + `"A dart flies out of the wall!"`, becomes visible |
| `Alarm` | No damage, but spawns 1–2 extra monsters of the current floor's table adjacent to the player, becomes visible |

| Task | Detail |
|------|--------|
| Placement | Generator rolls `3 + depth` traps on random floor tiles that are **not** in the spawn room, not on stairs, and not inside the treasure room (traps and treasure are separate rewards for separate detours) |
| Trigger site | In the movement pipeline, after the player successfully moves onto a tile, check `dlTraps` for a hidden trap at that position; if found, resolve the effect, set `tHidden = False`, and emit the appropriate `GameEvent` |
| Rendering | Hidden traps render as the underlying floor; visible traps render as `^` in red |
| FOV interaction | Traps are not tiles, so they do not block sight; the visible/hidden flag is purely for rendering |
| Monsters don't trigger traps | To keep monster AI pure and avoid pathing into a cleared trap being suspicious, only the player triggers traps in this milestone — a later pass can extend this |
| Save codec | `Trap` and `TrapKind` get `Binary` instances; codec version bumps are amortized with Step 2 |

**Acceptance:** Stepping on a hidden trap deals damage (or spawns
monsters), reveals the trap glyph, and writes the correct event to
`gsEvents`; reloaded levels remember which traps have been triggered.

---

#### Step 5 — Tests

| Module | Tests |
|--------|-------|
| `Game.Logic.DungeonSpec` | `prop_everyLevelHasAtLeastOneDoor` (except boss floor); `prop_treasureRoomExistsBelowBoss`; `prop_treasureRoomKeyIsReachable` (flood-fill from spawn, check the key is reachable without passing through the locked door); `prop_noTrapOnStairs`; `prop_noTrapInSpawnRoom` |
| `Game.Logic.MovementSpec` | bumping a closed door opens it and does not move the player; bumping a locked door without the key fails; bumping a locked door with the key consumes the key and opens the door |
| `Game.Logic.FOVSpec` | `prop_closedDoorBlocksFOV` (already implicit but make it explicit); open doors do not block FOV |
| `Game.GameStateSpec` | `applyAction Move` onto a hidden trap tile deals the expected damage and emits `EvPlayerHurt`; a revealed trap does not re-trigger on the next step |
| `Game.SaveSpec` | a level with a `Locked` door, a key in the inventory, and a revealed trap round-trips through `encodeSave`/`decodeSave`; forcing `dlTreasureRoom` and `dlTraps` fields from a decoded save to catch lazy-field regressions |

---

#### Suggested Implementation Order

1. **Place unlocked doors** (Step 1 only) — smallest possible diff that makes the README true; validates that the existing Door wiring works
2. **Traps** (Step 4) — orthogonal to keys/treasure, easy to add incrementally
3. **Locked doors + keys** (Step 2) — enables Step 3
4. **Treasure rooms** (Step 3) — composes keys + loot, lands the headline feature
5. **Save codec bump** — single version bump at the end (`DHSAVE03` → `DHSAVE04`) covering `KeyId`, `Trap`, and the new `DungeonLevel` fields
6. **Tests** — added alongside each step, not deferred

---

#### Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Locked treasure room is unreachable because the key is itself behind the locked door | Generator picks the key tile *after* flood-filling from spawn with the treasure door treated as a wall — the key must land in the reachable set |
| A closed door blocks the player from ever reaching the stairs | Spawn room and the room containing `StairsUp` never get closed doors on their outgoing corridors; a flood-fill sanity check in the generator rejects any layout where stairs are unreachable from spawn assuming all non-locked doors open |
| Traps make the early floors unfair | Scale trap count and damage with depth; depth 1 gets 3 traps max with only `Spikes`; `Alarm` and `Dart` unlock at depth 3+ |
| Save format churn | Land Steps 2–4 together behind a single `DHSAVE04` bump; pre-M15 saves refuse to load with `SaveWrongVersion`, matching the existing behaviour |
| HPC regression from the new fields | Every new `DungeonLevel` / `Trap` field gets at least one test that forces it (same pattern as the SaveMetadata fix in the M14 follow-ups) |

---

### Milestone 16: Monster vision and stealth

**Goal:** Monsters only pursue and attack the player when they can
actually see them. Today `Game.Logic.MonsterAI.monsterIntent` is
omniscient — every monster on the level homes in on `gsPlayerPos`
regardless of walls, closed doors, or distance. The `transparent`
predicate in `Game.Logic.FOV` already classifies closed doors as
opaque for the player's FOV, but the monster AI never consults it.
This milestone wires monsters through the same line-of-sight check,
so closed doors and rooms become genuine tactical cover.

**Out of scope** (follow-ups if the "fish in a barrel" fix feels too
simple in play):
- Monster *memory* — "last seen" position so a monster that lost
  sight of the player still walks to where they vanished
- Sound-based alerts — combat or door-bumping noise waking nearby
  monsters even outside line of sight
- Per-monster sight radius (rats see less than dragons)
- Monsters opening doors

---

#### Step 1 — Extract a line-of-sight primitive

The symmetric Bresenham check that powers `computeFOV` is currently
a local `canSee` / `losClear` pair inside `Game.Logic.FOV`. Extract
it into a top-level, exported function:

```haskell
-- Game.Logic.FOV
hasLineOfSight :: DungeonLevel -> Pos -> Pos -> Bool
```

`computeFOV` keeps working — it just calls the extracted function
instead of inlining the check. No FOV behaviour change for the
player.

---

#### Step 2 — Gate `monsterIntent` on visibility

| Task | Detail |
|------|--------|
| New constant | `monsterSightRadius :: Int = 8` in `Game.GameState` next to `fovRadius`. Start symmetric with the player so "if I can see it, it can see me" — gameplay can retune later without touching the pipeline |
| New parameter | `monsterIntent :: DungeonLevel -> Pos -> [Pos] -> Int -> Monster -> MonsterIntent` — the new `Int` is the sight radius. Passing it in keeps `MonsterAI` pure and the radius configurable from `GameState` |
| Visibility check | Before the existing attack/move/wait dispatch, compute `canSeePlayer`: any footprint tile of the monster is within `sightRadius` (Euclidean, matching `computeFOV`) **and** has a clear line of sight to the player via `hasLineOfSight`. Multi-tile bosses use an `any` over their footprint so the dragon is as perceptive as its largest silhouette |
| Short-circuit | If `canSeePlayer` is false, return `MiWait` immediately. The attack and move branches stay behind the check, which is why "adjacent through a wall" (an illegal edge case anyway) no longer attacks |

The attack branch survives for normal adjacent combat because
`hasLineOfSight` is trivially clear between Chebyshev-1 tiles (no
middle tiles for the Bresenham line to check).

---

#### Step 3 — Wire the radius through `processMonster`

`Game.GameState.processMonster` currently calls
`monsterIntent dl playerPos others m`. Change it to
`monsterIntent dl playerPos others monsterSightRadius m` and import
the new constant. No other call sites.

---

#### Step 4 — Tests

| Module | Test |
|--------|------|
| `Game.Logic.FOVSpec` | `hasLineOfSight` is clear through an open room, blocked by a wall between the endpoints, blocked by a closed door, clear through an open door. Symmetry: `hasLineOfSight dl a b == hasLineOfSight dl b a` |
| `Game.Logic.MonsterAISpec` | Existing `MiMove` / `MiAttack` tests pick up the new `sightRadius` parameter — they already run in an open 10x10 room so LOS is trivially clear; just thread the parameter through |
| `Game.Logic.MonsterAISpec` | **New:** monster in a separate sub-room with a `Door Closed` between it and the player → `MiWait` |
| `Game.Logic.MonsterAISpec` | **New:** same layout with `Door Open` → `MiMove` toward the player |
| `Game.Logic.MonsterAISpec` | **New:** monster in LOS but Euclidean distance > `sightRadius` → `MiWait` (was `MiMove` pre-M16) |
| `Game.GameStateSpec` | `processMonster` integration test: a rat placed in an adjacent closed-off sub-room does not move on the player's turn |

---

#### Acceptance

- A monster on the other side of a closed door stands still turn
  after turn while the player moves in the adjacent room.
- Opening the door causes the monster to begin pursuing on the next
  player turn.
- Monsters farther than `monsterSightRadius` from the player take no
  pursuit action (previous behaviour: global pursuit).
- All pre-M16 tests still pass after the `monsterIntent` signature
  change.

---

#### Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Levels feel empty because the player can wall-hack their way past every monster | Follow-up milestone for monster memory + sound alerts; document the limitation in the README so the current behaviour is a known, bounded simplification |
| Breaking change to `monsterIntent` ripples through callers | Only one caller (`processMonster`) and one test module touch the function; the change is mechanical |
| Bosses become trivial by kiting around corners | The dragon's footprint is large enough that at least one of its tiles usually has LOS; if playtesting shows the boss losing sight too easily, raise `monsterSightRadius` for boss encounters specifically (single-line change) |
| Performance regression from per-monster LOS checks | One Bresenham walk per monster per turn is O(max dx dy) ≈ 20 operations at worst; with ≤ 10 monsters on a level this is ~200 ops/turn, well below anything measurable |

---

### Milestone 17: Fun Pass — Sustain, Run Stats, and Bows

**Goal:** Make combat feel sustainable and runs feel memorable. Combat
currently leaves the player at 3-5 HP after fighting rats and goblins
with no recovery path between encounters — the only HP source is
monster drops. Dash and door-closing already enable tactical retreat
and feel good, so the answer is to reward that retreat with time-based
sustain instead of buffing combat directly. On top of that, we
gamify full runs by tracking time-to-victory, potions used, and saves
used, so beating the dragon produces a memorable score card. Bows &
arrows are included as an optional final slice for range variety on
deeper floors.

**Non-goals / hard constraints:**
- `playerDash` (`src/Game/GameState.hs:992-1014`) and `playerCloseDoor`
  (`src/Game/GameState.hs:947-975`) must NOT be touched — they are the
  tactical backbone this milestone is built around.
- All state stays in `GameState`; pure-functional style preserved.
- Each slice that changes the persisted shape bumps `saveMagic` in
  `src/Game/Save/Types.hs:112-113`. `GameState` encoding is
  auto-derived via `StandaloneDeriving` in `src/Game/Save.hs:141-207`,
  so new fields serialize automatically once the magic is bumped.

Central hook: `tickPlayerTurn` (`src/Game/GameState.hs:1564-1567`)
already runs on every turn-advancing action. It is where regen,
turn-counter, and chest-timer ticks belong.

---

#### Step 1A — Passive HP regen (combat-gated)

+1 HP every 12 turns while no hostile monster is in the player's FOV.
Never exceeds `sMaxHP`. Encourages retreat-and-recover via dash + door
close.

| Task | Detail |
|------|--------|
| New field | `gsRegenCounter :: !Int` next to `gsDashCooldown` (~line 223); init `0` in `mkGameState` (~line 439) |
| New constant | `regenInterval :: Int = 12` near `dashCooldownTurns` (~line 375) |
| New helper | `tickRegen :: GameState -> GameState` — early-return on full HP, `gsDead`, or `gsVictory`; "safe" = no monster position intersects `gsVisible gs` (reuse existing FOV set); not safe → reset counter; safe → increment; at `>= regenInterval`, +1 HP and reset |
| Wire-up | In `tickPlayerTurn`, compose `tickRegen . <existing dash tick>`. Do NOT inline into dash logic |

**Tests (`test/Game/Logic/` or equivalent):**

| Module | Test |
|--------|------|
| Regen tests | `tickRegen` with full HP is a no-op |
| Regen tests | `tickRegen` with `gsDead` / `gsVictory` is a no-op |
| Regen tests | 12 consecutive ticks with no monsters in FOV → +1 HP, counter reset |
| Regen tests | Tick with a monster position inside `gsVisible` → counter reset to 0, no HP change |
| Regen tests | Regen never exceeds `sMaxHP` (multi-tick loop stopping at cap) |

---

#### Step 1B — Respawning chests

Time is a resource. Exactly 1 chest on depth 1; no chests on depths
2-3; starting at depth 4, each non-boss floor has a 60% chance of 1-2
chests in non-starting rooms. Chests refill after ~100 turns.

**New module:** `src/Game/Logic/Chest.hs`

```haskell
data ChestState = ChestFull !Item | ChestEmpty !Int  -- Int = turns until refill
data Chest      = Chest { chestPos :: !Pos, chestState :: !ChestState }

chestLootTable    :: [(Int, Item)]      -- mostly HealingMinor, medium HealingMajor, rare weapon
rollChestLoot     :: StdGen -> (Item, StdGen)
chestRespawnTurns :: Int                -- 100
```

Reuse `pickWeighted` from `src/Game/Logic/Loot.hs:75-88` (export it from
`Loot.hs` if currently private).

**GameState changes (`src/Game/GameState.hs`):**

| Task | Detail |
|------|--------|
| New field | `gsChests :: ![Chest]` near `gsItemsOnFloor`; init `[]` in `mkGameState` |
| Parked level | Add `plChests :: ![Chest]` to `ParkedLevel` (~line 140-147); thread through `parkCurrent` / `loadParked` (~line 1305-1333) |
| `newGame` | On initial depth 1 generation, roll one `ChestFull` in a non-starting room (~line 467-491) |
| `generateAndEnter` | When `depth >= 4` and not boss floor, 60% chance place 1-2 chests via threaded RNG (~line 1348-1400) |
| Re-entry refill | In `playerAscend` / `playerDescend` (~line 1519-1552) after `loadParked`: walk `gsChests`; any `ChestEmpty n` with `n <= 0` → re-roll via `rollChestLoot` back to `ChestFull` |
| Tick timers | In `tickPlayerTurn`, decrement `ChestEmpty n` by 1 per turn on the *current* floor (min-clamped at 0). Parked floors don't tick — refill check happens on re-entry |
| Bump-to-open | In `applyAction` `Move` branch (~line 797-839) before `monsterAt`/`npcAt`: detect chest at destination; on `ChestFull item` add to inventory (or drop on floor if bag full), flip to `ChestEmpty chestRespawnTurns`, push message, call `processMonsters`. Matches bump-to-open doors |
| Render | In `src/Game/Render.hs` (~line 250) overlay `=` for `ChestFull` and dim `=` for `ChestEmpty`, rendered under items/monsters |
| Save | Bump `saveMagic`; add `Binary`/`Generic` derivations for `Chest` and `ChestState` in `src/Game/Save.hs:141-207` |

**Tests:**

| Module | Test |
|--------|------|
| `Game.Logic.ChestSpec` | `rollChestLoot` deterministic given a fixed seed; distribution hits each table entry under many seeds |
| `Game.Logic.ChestSpec` | Chest tick: `ChestEmpty n` → `ChestEmpty (n-1)`, clamped at 0; `ChestFull` is a no-op |
| `Game.GameStateSpec` | Bump-to-open transition: `ChestFull item` → item in inventory + `ChestEmpty chestRespawnTurns`; full-bag path drops to `gsItemsOnFloor` instead |
| `Game.GameStateSpec` | Re-entry refill: `ChestEmpty 0` in `plChests` becomes `ChestFull` after `loadParked`; `ChestEmpty 5` stays empty |
| `Game.GameStateSpec` | `newGame` always seeds exactly one chest on depth 1; `generateAndEnter` places zero chests for depths 2-3 and ≥1 chests at depth 4+ only when the RNG roll succeeds (use forced seeds) |
| `Game.SaveSpec` | Roundtrip: encode/decode a `GameState` containing both `ChestFull` and `ChestEmpty n` → equal after roundtrip |

**Manual sanity (one short run):** start game, bump the depth-1 chest
→ glyph dims, item appears. Everything else is covered by tests.

---

#### Step 2 — Run stats / gamification

Track time-to-victory, potions used, and saves used. Show them in the
HUD and on the victory modal with a computed "rank".

**New fields (`src/Game/GameState.hs` ~line 223):**

```haskell
gsTurnsElapsed :: !Int
gsPotionsUsed  :: !Int
gsSavesUsed    :: !Int
gsFinalTurns   :: !(Maybe Int)  -- frozen on victory
```

Initialize all to `0`/`Nothing` in `mkGameState` (~line 439).

**Increment points:**

| Location | Change |
|----------|--------|
| `tickPlayerTurn` | `gsTurnsElapsed += 1`, gated on `not gsDead && isNothing gsFinalTurns` so the counter freezes on death or victory |
| `playerUseItem` `IPotion` branch (~line 1272) | `gsPotionsUsed += 1` |
| `playerAttack` where `gsVictory` flips to `True` (~line 1206) | Set `gsFinalTurns = Just gsTurnsElapsed` |
| `src/Game/UI/Prompt.hs:194` (`doQuicksave`) | Bump `gsSavesUsed` on in-memory state **before** `writeSave` so the saved blob records its own save |
| `src/Game/UI/SaveMenu.hs:224` (`performSaveAt`) | Same bump-before-write pattern |

**Display:**

| Location | Change |
|----------|--------|
| `drawStatus` (`src/Game/Render.hs:259-275`) | Append `T: N` (turns — use `fromMaybe gsTurnsElapsed gsFinalTurns`) and `P: N` (potions) to the status line |
| `drawVictoryModal` (`src/Game/Render.hs:576-587`) | Take `GameState` as param; render turns, potions used, saves used, final depth (`dlDepth (gsLevel gs)`), player level, and a computed rank. Add helper `runRank :: GameState -> String` bucketing by `(finalTurns, potionsUsed, savesUsed)` — e.g. `< 1500 turns && <= 3 potions && 0 saves` → "Legendary"; looser tiers → "Heroic", "Victor". Update `drawVictoryModal`'s call site in `drawUI` |

**Save:** bump `saveMagic` again. No manual encoder changes — the four
new fields ride on derived `Binary`.

**Tests:**

| Module | Test |
|--------|------|
| Tick tests | `tickPlayerTurn` increments `gsTurnsElapsed` exactly once per call; gated off when `gsDead` or `gsFinalTurns /= Nothing` |
| Inventory tests | `playerUseItem` on a potion increments `gsPotionsUsed` by 1; on a weapon/armor/key, it does not |
| Combat tests | Victory transition: flipping `gsVictory` to `True` sets `gsFinalTurns = Just gsTurnsElapsed` and subsequent ticks leave both frozen |
| Render helper | `runRank` bucket boundaries: table-driven test over representative `(turns, potions, saves)` tuples |
| Save tests | Save counter: calling the save helper path on an in-memory state bumps `gsSavesUsed` before serialization (test the pure state-transform, not IO) |
| `Game.SaveSpec` | Roundtrip: encode/decode preserves all four counters |

**Manual sanity:** render `drawVictoryModal` on a fixture `GameState`
via a golden-style test rather than winning the game manually.

---

#### Step 3 — Bows & arrows (OPTIONAL, can be skipped)

Self-contained — no step in this milestone depends on it. Melee flow
stays unchanged. Skip this step entirely if the first three already
feel good.

**3A. Types & loot**

| File | Change |
|------|--------|
| `src/Game/Types.hs` (~line 321-324) | Extend `Weapon`: add `Bow` |
| `src/Game/Types.hs` `Inventory` (~line 364-368) | Add `invArrows :: !Int` (stackable ammo counter; cleaner than per-arrow slots) |
| `src/Game/Logic/Inventory.hs` | `Bow` entry in weapon stat/damage lookup — `Bow` gives 0 melee bonus; ranged bonus comes from the fire action itself |
| `src/Game/Logic/Loot.hs:34-58` | Add `(1, IWeapon Bow)` to Orc table and arrows drop row to Goblin + Orc |
| `src/Game/Logic/Chest.hs` (from Step 1B) | Add `Bow` + arrow refill entries to the depth-4+ chest loot table |

**3B. Fire action**

| File | Change |
|------|--------|
| `src/Game/Types.hs` `GameAction` (~line 158-182) | Add `Fire !Dir` constructor |
| `DirectionalAction` (`src/Game/GameState.hs:236-238`) | Add `DirFire` parallel to `DirCloseDoor` |
| `src/Game/Input.hs` | Bind `f` to set `gsAwaitingDirection = Just DirFire`; second keystroke yields `Fire d`. Reuses the two-step input path `DirCloseDoor` already established — no new modal infra |
| NEW `src/Game/Logic/Ranged.hs` | `fireArrow :: Dir -> GameState -> GameState`. Preconditions: `invWeapon == Just Bow`, `invArrows > 0`. Failures push a message and do NOT advance the turn (parallels dash-on-cooldown). Walk up to `arrowRange = 8` tiles along `dir`, stopping on first wall/closed-or-locked door/NPC, or first monster hit. Reuse `isWalkable`/`tileAt`. On hit: reuse `Combat.resolveAttack`. Extract a shared helper `applyHitResult :: GameState -> Monster -> CombatResult -> GameState` out of `playerAttack` (~line 1100-1224) so the kill/loot/XP/victory flow is shared (no duplicated boss-victory logic). Always decrement `invArrows`, then `processMonsters` |
| `src/Game/GameState.hs` `applyAction` (~line 786-796) | `Fire dir -> processMonsters (fireArrow dir gs)` |
| `src/Game/Render.hs` | Show `Bow` and arrow count in inventory modal (~line 298-319); add `f` to help modal (~line 617) |
| Save | Bump `saveMagic` |

**Tests:**

| Module | Test |
|--------|------|
| `Game.Logic.RangedSpec` | `fireArrow` with no bow equipped → state unchanged except for a message; turn NOT advanced |
| `Game.Logic.RangedSpec` | `fireArrow` with bow but 0 arrows → same |
| `Game.Logic.RangedSpec` | `fireArrow` hits first monster along direction (not monsters behind it) |
| `Game.Logic.RangedSpec` | `fireArrow` stops at walls, closed doors, locked doors, and NPCs without dealing damage |
| `Game.Logic.RangedSpec` | Arrow decrement happens exactly once per successful fire |
| `Game.GameStateSpec` | Shared `applyHitResult` helper: killing the dragon via ranged also sets `gsVictory` and `gsFinalTurns` (regression guard against boss-victory code path being duplicated instead of shared) |
| `Game.SaveSpec` | Roundtrip: `Bow` weapon and `invArrows` count preserved |

**Manual sanity:** one quick in-game fire against a rat to confirm the
input binding and rendering.

---

#### Recommended execution order

1. **Step 1A** (regen) — smallest diff, ship first, no save bump yet.
2. **Step 2** (run stats) — save bump #1. Merge with 1A's save bump
   if both land together.
3. **Step 1B** (chests) — save bump #2, new `Game.Logic.Chest`
   module.
4. **Step 3** (bows) — save bump #3. OPTIONAL; stop here if the
   first three already feel good.

Each step is independently testable and shippable.

---

#### Verification strategy

The pure functional design makes this easy: almost every behavior
lives in a pure `GameState -> GameState` (or similar) function, so the
primary verification is **unit tests** placed alongside existing tests
in `test/`. Each step has its own test list inline above. No
exhaustive manual playthrough required.

**Unit tests cover:**
- Tick-based logic: regen gating, chest refill timers, turn counter,
  victory freeze.
- State transforms: bump-to-open chests, potion/save counter
  increments, ranged fire stop conditions.
- Loot determinism and save roundtrips across all new fields.

**Minimal manual checks (UI wiring only):**
- HUD status line renders the new counters (one glance).
- Chest glyph `=` appears on depth 1 (one bump).
- Victory modal renders final stats (prefer a golden-style test on
  `drawVictoryModal` with a fixture `GameState` over an actual boss
  fight).
- (Step 3) `f` + direction input path fires an arrow.

**Build check:** `cabal build` and `cabal test` after each step.

---

#### Acceptance

- After taking damage, retreating out of monster FOV and waiting ~12
  turns restores HP by 1 (and keeps ticking).
- Depth 1 shows exactly one chest glyph; bumping it yields a potion
  or weapon.
- Leaving a looted floor, playing elsewhere for 100+ turns, and
  returning refills the chest.
- HUD shows turns-elapsed and potions-used counters updating in
  real time.
- Quicksaving twice and winning the game shows `saves: 2` on the
  victory modal along with final turns, potions used, final depth,
  player level, and a rank string.
- (If Step 3 shipped) equipping a bow and pressing `f` + direction
  fires an arrow that hits the first monster in line and decrements
  `invArrows`; firing without a bow or arrows produces a message and
  does not advance the turn.

---

#### Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Regen trivializes combat by removing the cost of fights | Interval is intentionally slow (12 turns per HP) and gated on "no hostiles in FOV" — the player still has to disengage and burn tactical turns. Tune `regenInterval` up if playtesting shows runs becoming passive |
| Chest respawn timer feels like pointless waiting | 100 turns is roughly one floor's worth of exploration — the intended loop is "descend, explore, come back", not "sit on a chest". If it feels grindy, shorten the timer or allow partial refills |
| Save counter discourages normal saving | That is the point — the rank is a scoreboard nudge, not a punishment. Saving still works identically; only the victory rank sees the counter |
| Save format migrations pile up (three bumps across the milestone) | Batch 1A + 2 into a single bump if they ship together; otherwise each bump is mechanical since `StandaloneDeriving` handles encoding |
| Bow action breaks the bump-to-attack invariant that tests rely on | `fireArrow` is a distinct action constructor; the `Move` branch is untouched. Existing melee tests should pass unchanged |
| Extracting `applyHitResult` from `playerAttack` introduces regressions in boss-kill path | Extraction is mechanical (copy the kill/loot/XP/victory block into a helper taking the damage outcome). The existing boss-kill test and a new ranged-boss-kill test together catch any divergence |

---

## Testing Strategy

### Property-Based Tests (QuickCheck)

Every module in `Game/Logic/` exports pure functions. For each, define properties:

```haskell
-- Example: CombatSpec.hs
module Game.Logic.CombatSpec where

import Test.Hspec
import Test.QuickCheck
import Game.Logic.Combat
import Game.Types

-- Arbitrary instances for game types
instance Arbitrary Stats where
  arbitrary = Stats
    <$> chooseInt (1, 200)    -- hp
    <*> chooseInt (1, 200)    -- maxHP
    <*> chooseInt (1, 50)     -- attack
    <*> chooseInt (0, 40)     -- defense
    <*> chooseInt (1, 20)     -- speed

spec :: Spec
spec = describe "Combat" $ do
  it "damage is always positive on hit" $ property $
    \gen atk def ->
      case fst (resolveAttack gen atk def) of
        Hit (Damage d) -> d > 0
        CriticalHit (Damage d) -> d > 0
        _ -> True   -- miss/kill are fine

  it "armor reduces damage" $ property $
    \gen atk (Positive extra) ->
      let def0 = Stats 100 100 10 0 10
          def1 = def0 { sDefense = extra }
      in resultDamage (fst $ resolveAttack gen atk def0)
           >= resultDamage (fst $ resolveAttack gen atk def1)

  it "dead entities have 0 or less HP" $ property $
    \gen atk def ->
      case fst (resolveAttack gen atk def) of
        Kill -> applyDamage def (extractDamage $ resolveAttack gen atk def)
                  & \s -> sHP s <= 0
        _ -> True
```

### Golden Tests (Rendering)

Since the rendering is `GameState → Grid (Char, Attr)`, test the grid output:

```haskell
-- "Given a 5x5 room with player at (2,2), the output grid has '@' at (2,2)"
-- "Walls render as '#', floors as '.', monsters as their letter"
```

### Smoke Tests (Integration)

Run a sequence of actions through the full Apecs world and assert final state:

```haskell
-- "Player moves right 3 times from (1,1) in open room → position is (4,1)"
-- "Player bumps into monster → monster HP decreases"
```

---

## Cabal File

```cabal
cabal-version:   3.0
name:            dungeon-haskell
version:         0.1.0.0
license:         BSD-3-Clause
build-type:      Simple

common shared
  default-language: GHC2021
  default-extensions:
    OverloadedStrings
    ScopedTypeVariables
    TypeFamilies
    MultiParamTypeClasses
    TemplateHaskell
    FlexibleInstances
  ghc-options: -Wall -Wno-orphans
  build-depends:
    , base           >= 4.16 && < 5
    , apecs          >= 0.9
    , linear         >= 1.21
    , containers     >= 0.6
    , vector         >= 0.13
    , random         >= 1.2
    , MonadRandom    >= 0.6
    , mtl            >= 2.3

library
  import:          shared
  hs-source-dirs:  src
  exposed-modules:
    Game.Types
    Game.Components
    Game.World
    Game.Systems
    Game.Render
    Game.Input
    Game.Audio              -- M8: IO shell over proteaaudio-sdl
    Game.Logic.Combat
    Game.Logic.Movement
    Game.Logic.FOV
    Game.Logic.Dungeon
    Game.Logic.Inventory
    Game.Logic.Loot
    Game.Logic.Quest
    Game.Logic.Progression  -- M7: XP + level-ups
  build-depends:
    , brick          >= 2.1
    , vty            >= 6.1
    , vty-crossplatform >= 0.4
    , proteaaudio-sdl >= 0.10   -- M8; needs libsdl2-dev on Linux

executable dungeon-haskell
  import:          shared
  hs-source-dirs:  app
  main-is:         Main.hs
  build-depends:
    , dungeon-haskell
    , brick
    , vty
    , vty-crossplatform
  ghc-options:     -threaded -rtsopts

test-suite dungeon-haskell-test
  import:          shared
  type:            exitcode-stdio-1.0
  hs-source-dirs:  test
  main-is:         Spec.hs
  other-modules:
    Game.Logic.CombatSpec
    Game.Logic.MovementSpec
    Game.Logic.FOVSpec
    Game.Logic.DungeonSpec
    Game.Logic.InventorySpec
    Game.Logic.QuestSpec
    Game.Logic.ProgressionSpec   -- M7
    Game.GameStateSpec           -- M8: event-list properties
  build-depends:
    , dungeon-haskell
    , hspec          >= 2.11
    , QuickCheck     >= 2.14
    , hspec-discover >= 2.11
  build-tool-depends:
    hspec-discover:hspec-discover
  ghc-options:     -threaded -rtsopts
```

---

## Getting Started

```bash
# Create project
mkdir dungeon-haskell && cd dungeon-haskell

# Init with cabal (or copy the cabal file above)
cabal init --minimal

# Create directory structure
mkdir -p app src/Game/Logic test/Game/Logic

# Build & test cycle
cabal build
cabal test
cabal run dungeon-haskell
```

---

## Rendering Approach (Brick)

Brick renders the dungeon as a grid of styled characters:

```haskell
-- Render.hs sketch
renderGame :: GameState -> Widget Name
renderGame gs = 
  vBox [ renderDungeon (gsDungeon gs) (gsPlayerPos gs) (gsVisibleTiles gs)
       , hBorder
       , renderStats (gsPlayerStats gs)
       , renderMessages (gsMessages gs)
       ]

renderDungeon :: DungeonLevel -> Pos -> Set Pos -> Widget Name
renderDungeon dl playerPos visible =
  vBox [ hBox [ renderTile x y | x <- [0..dlWidth dl - 1] ]
       | y <- [0..dlHeight dl - 1]
       ]
  where
    renderTile x y
      | V2 x y == playerPos     = withAttr attrPlayer (str "@")
      | V2 x y `Set.member` visible =
          case tileAt dl (V2 x y) of
            Just Floor      -> withAttr attrFloor (str ".")
            Just Wall       -> withAttr attrWall  (str "#")
            Just (Door Open)-> withAttr attrDoor  (str "/")
            Just StairsDown -> withAttr attrStairs(str ">")
            _               -> str " "
      | V2 x y `Set.member` explored = withAttr attrFog (str "·")
      | otherwise = str " "
```

This is deliberately simple. All the interesting work is in the logic layer.

---

## Death Penalty Design

On player death:
1. Lose 50% of carried gold
2. Lose 1 random non-equipped item
3. Respawn at level 1 entrance with full HP
4. Current level monsters/items reset
5. Quest progress preserved (quests don't fail on death)

All of this is testable as pure functions:

```haskell
applyDeathPenalty :: StdGen -> Inventory -> Gold -> (Inventory, Gold, StdGen)

prop_deathPenaltyHalvesGold :: StdGen -> Positive Int -> Bool
prop_deathPenaltyHalvesGold gen (Positive g) =
  let (_, Gold g', _) = applyDeathPenalty gen emptyInventory (Gold g)
  in g' == g `div` 2

prop_deathPenaltyLosesOneItem :: StdGen -> NonEmpty Item -> Bool
prop_deathPenaltyLosesOneItem gen items =
  let inv = inventoryFromList (toList items)
      (inv', _, _) = applyDeathPenalty gen inv (Gold 0)
  in inventoryCount inv' == inventoryCount inv - 1
```

---

## Notes

- **Turn-based simplifies everything.** No game loop timing, no frame rate,
  no physics. Each player action is one "tick". Monster AI runs after player.
- **Brick handles resize.** Terminal roguelikes naturally adapt to terminal size.
- **Start with Milestone 1.** Get `@` moving in a room with 4 passing tests
  before touching anything else. Resist the urge to design the full game upfront.
- **QuickCheck Arbitrary instances are your secret weapon.** Invest time in good
  generators for Stats, DungeonLevel, Inventory — they pay off exponentially
  as the game grows.
- **The pure logic layer is portable.** If you ever want a web frontend (via
  GHCJS/Miso) or SDL2 graphics, only Render.hs and Input.hs change.


