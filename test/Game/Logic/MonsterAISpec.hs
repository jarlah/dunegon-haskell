module Game.Logic.MonsterAISpec (spec) where

import qualified Data.Vector as V
import Linear (V2(..))
import Test.Hspec

import Game.Logic.MonsterAI
import Game.Types

-- | A 10x10 open room with walls around the outer edge — same
--   shape MovementSpec and FOVSpec use. The monster AI only cares
--   about walkability of individual tiles, so this is enough to
--   drive every branch of 'monsterIntent'.
testRoom :: DungeonLevel
testRoom = DungeonLevel
  { dlWidth  = 10
  , dlHeight = 10
  , dlDepth  = 1
  , dlTiles  = V.generate (10 * 10) mkTile
  , dlRooms  = [Room 1 1 8 8]
  }
  where
    mkTile i =
      let (y, x) = i `divMod` 10
      in if x == 0 || y == 0 || x == 9 || y == 9 then Wall else Floor

-- | Default sight radius used by every 'monsterIntent' test that
--   doesn't specifically care about the distance cut-off. Generous
--   enough that every in-test geometry fits comfortably inside it
--   — the sight radius itself is only exercised by the dedicated
--   "beyond the sight radius" case.
testSightRadius :: Int
testSightRadius = 20

-- | A 10x5 level with two 3x3 rooms joined by a single-tile
--   corridor on row 2, with a door in the middle of that corridor
--   at (5,2). The door tile is parameterised so the same geometry
--   drives both the "closed door blocks LOS" and "open door admits
--   LOS" tests.
--
--   Layout (rows 0..4, cols 0..9):
--
--       ##########
--       #...##...#
--       #....D...#   <- door at (5,2)
--       #...##...#
--       ##########
--
--   Cols 4-5 on rows 1 and 3 are wall, so the only horizontal line
--   of sight between the west room and the east room runs along
--   row 2 through the door tile.
twoRoomLevel :: Tile -> DungeonLevel
twoRoomLevel doorTile = DungeonLevel
  { dlWidth  = 10
  , dlHeight = 5
  , dlDepth  = 1
  , dlTiles  = V.generate (10 * 5) mkTile
  , dlRooms  = [Room 1 1 3 3, Room 6 1 3 3]
  }
  where
    mkTile i =
      let (y, x) = i `divMod` 10
      in if y == 0 || y == 4 || x == 0 || x == 9
           then Wall
           else if y == 2
             then if x == 5 then doorTile else Floor
             else if x == 4 || x == 5
               then Wall
               else Floor

-- | A 3x3 bottleneck: a single floor tile surrounded by walls on
--   all sides (the rat stands at the floor tile with no legal
--   moves). Used to drive the 'MiWait' fallback.
boxRoom :: Monster -> DungeonLevel
boxRoom m = DungeonLevel
  { dlWidth  = 3
  , dlHeight = 3
  , dlDepth  = 1
  , dlTiles  = V.generate (3 * 3) mkTile
  , dlRooms  = [Room 1 1 1 1]
  }
  where
    mkTile i =
      let (y, x) = i `divMod` 3
      in if V2 x y == mPos m then Floor else Wall

spec :: Spec
spec = describe "Game.Logic.MonsterAI" $ do

  describe "chebyshev" $ do
    it "is zero for equal points" $
      chebyshev (V2 3 4) (V2 3 4) `shouldBe` 0

    it "is the max of the axis deltas" $ do
      chebyshev (V2 0 0) (V2 3 1) `shouldBe` 3
      chebyshev (V2 0 0) (V2 1 3) `shouldBe` 3
      chebyshev (V2 2 2) (V2 5 6) `shouldBe` 4

    it "is symmetric" $
      chebyshev (V2 1 2) (V2 4 6) `shouldBe` chebyshev (V2 4 6) (V2 1 2)

  describe "monsterIntent" $ do

    it "attacks when the player is orthogonally adjacent" $ do
      let rat       = mkMonster Rat (V2 2 2)
          playerPos = V2 2 3   -- south of the rat, Chebyshev 1
      monsterIntent testRoom playerPos [] testSightRadius rat `shouldBe` MiAttack

    it "attacks when the player is diagonally adjacent" $ do
      let rat       = mkMonster Rat (V2 2 2)
          playerPos = V2 3 3   -- southeast of the rat, Chebyshev 1
      monsterIntent testRoom playerPos [] testSightRadius rat `shouldBe` MiAttack

    it "moves toward the player when out of reach and the path is clear" $ do
      let rat       = mkMonster Rat (V2 2 2)
          playerPos = V2 5 5   -- Chebyshev 3, not adjacent
      case monsterIntent testRoom playerPos [] testSightRadius rat of
        MiMove p ->
          -- The chosen move must actually get closer to the player.
          chebyshev p playerPos < chebyshev (mPos rat) playerPos
            `shouldBe` True
        other    ->
          expectationFailure ("expected MiMove, got " ++ show other)

    it "waits when every neighbor tile is blocked" $ do
      -- Sole-floor-tile room: the rat is at the one walkable tile,
      -- so none of its eight neighbors is walkable, and there are
      -- no legal candidate moves. With the player far away it must
      -- fall through to 'MiWait'.
      let rat       = mkMonster Rat (V2 1 1)
          dl        = boxRoom rat
          -- Player is outside any walkable tile; Chebyshev distance
          -- is > 1 so the attack branch is skipped and the move
          -- search runs. All candidates fail canStandOn, so the
          -- intent collapses to Wait.
          playerPos = V2 10 10
      monsterIntent dl playerPos [] testSightRadius rat `shouldBe` MiWait

    it "treats another monster's tile as blocked" $ do
      -- Two rats in a line toward the player: the back rat wants
      -- to step west toward the player but the front rat blocks
      -- the direct tile. It should still move (sidestep), just
      -- not onto the blocked position.
      let frontRat  = mkMonster Rat (V2 3 2)
          backRat   = mkMonster Rat (V2 4 2)
          blocked   = monsterTiles frontRat
          playerPos = V2 1 2
      case monsterIntent testRoom playerPos blocked testSightRadius backRat of
        MiMove p ->
          p `notElem` blocked `shouldBe` True
        MiAttack ->
          expectationFailure "backRat is Chebyshev 3 from player, should not attack"
        MiWait   ->
          expectationFailure "backRat has legal diagonal moves, should not wait"

    -- ----------------------------------------------------------------
    -- Milestone 16: monster vision / stealth
    -- ----------------------------------------------------------------

    it "waits when the player is beyond the sight radius" $ do
      -- Rat at (2,2), player at (8,8) in an open 10x10 room: LOS is
      -- trivially clear but the Euclidean distance is sqrt(72) ~ 8.5,
      -- which exceeds the short sight radius of 5. Pre-M16 the rat
      -- would have chased from any distance.
      let rat        = mkMonster Rat (V2 2 2)
          playerPos  = V2 8 8
          shortSight = 5
      monsterIntent testRoom playerPos [] shortSight rat `shouldBe` MiWait

    it "waits when a closed door stands between it and the player" $ do
      -- The rat is in the east room, the player in the west room,
      -- with a single closed door on the only straight LOS line.
      -- The rat cannot see the player and must stay put.
      let rat       = mkMonster Rat (V2 7 2)
          playerPos = V2 2 2
      monsterIntent (twoRoomLevel (Door Closed)) playerPos [] testSightRadius rat
        `shouldBe` MiWait

    it "chases when the door between it and the player is open" $ do
      -- Same geometry, door now Open: LOS is clear, so the rat
      -- should start sliding west toward the player.
      let rat       = mkMonster Rat (V2 7 2)
          playerPos = V2 2 2
      case monsterIntent (twoRoomLevel (Door Open)) playerPos [] testSightRadius rat of
        MiMove p ->
          chebyshev p playerPos < chebyshev (mPos rat) playerPos
            `shouldBe` True
        other    ->
          expectationFailure ("expected MiMove, got " ++ show other)
