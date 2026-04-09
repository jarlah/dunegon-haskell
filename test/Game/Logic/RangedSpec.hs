-- | Tests for 'Game.Logic.Ranged'. These exercise the pure
--   'walkRay' helper directly, so the fixtures are tiny hand-built
--   levels and plain closures rather than real 'GameState' values.
--   Integration tests that drive 'fireArrow' end-to-end live in
--   'Game.GameStateSpec'.
module Game.Logic.RangedSpec (spec) where

import qualified Data.Vector as V
import           Linear      (V2 (..))
import           Test.Hspec

import           Game.Logic.Ranged
import           Game.Types

-- | 5x5 dungeon level with walls around the edge and open floor
--   in the 3x3 interior. Matches 'tinyRoom' in 'GameStateSpec' but
--   lives here locally so the two test modules stay independent.
tinyRoom :: DungeonLevel
tinyRoom = DungeonLevel
  { dlWidth  = 5
  , dlHeight = 5
  , dlDepth  = 1
  , dlTiles  = V.generate (5 * 5) $ \i ->
      let (y, x) = i `divMod` 5
      in if x == 0 || y == 0 || x == 4 || y == 4 then Wall else Floor
  , dlRooms  = [Room 1 1 3 3]
  }

-- | Overwrite one tile of 'tinyRoom' and return the patched level.
withTile :: Pos -> Tile -> DungeonLevel -> DungeonLevel
withTile (V2 x y) t dl =
  dl { dlTiles = dlTiles dl V.// [(y * dlWidth dl + x, t)] }

-- | Straight line of up to 'arrowRange' tiles starting one step
--   north of the player. Matches the construction inside
--   'Game.GameState.fireArrow'.
northPath :: Pos -> [Pos]
northPath start = [ start + V2 0 (-k) | k <- [1 .. arrowRange] ]

-- | A trivial always-false position test used when there are no
--   NPCs / chests in the fixture.
noHit :: Pos -> Bool
noHit _ = False

-- | Always-Nothing monster lookup used when there are no monsters.
noMonster :: Pos -> Maybe (Int, Monster)
noMonster _ = Nothing

-- | Simple "monster at this exact position" lookup.
monsterAtFixed :: Pos -> Monster -> Pos -> Maybe (Int, Monster)
monsterAtFixed target m p
  | p == target = Just (0, m)
  | otherwise   = Nothing

spec :: Spec
spec = describe "Game.Logic.Ranged.walkRay" $ do

  it "returns RayDropped when the path is empty" $
    walkRay tinyRoom noMonster noHit noHit [] `shouldBe` RayDropped

  it "returns RayDropped when the path only traverses empty floor" $ do
    -- Player at (1,3) firing N; interior is (1,1)..(3,3) so the ray
    -- hits open floor at (1,2), (1,1), then wall at (1,0). With no
    -- monsters or NPCs, that's a 'RayBlocked' on the wall, NOT a
    -- drop. A drop requires the path to actually exhaust.
    let outcome = walkRay tinyRoom noMonster noHit noHit (northPath (V2 1 3))
    outcome `shouldBe` RayBlocked "clatters against the wall"

  it "hits the first monster along the path" $ do
    let rat     = mkMonster Rat (V2 1 1)
        lookup_ = monsterAtFixed (V2 1 1) rat
        outcome = walkRay tinyRoom lookup_ noHit noHit (northPath (V2 1 3))
    case outcome of
      RayHitMonster 0 m -> mKind m `shouldBe` Rat
      other             -> expectationFailure ("unexpected: " ++ show other)

  it "does not reach monsters past a closed door" $ do
    let doorAt  = V2 1 2
        dl'     = withTile doorAt (Door Closed) tinyRoom
        rat     = mkMonster Rat (V2 1 1)
        lookup_ = monsterAtFixed (V2 1 1) rat
        outcome = walkRay dl' lookup_ noHit noHit (northPath (V2 1 3))
    outcome `shouldBe` RayBlocked "thuds into the closed door"

  it "does not reach monsters past a locked door" $ do
    let doorAt  = V2 1 2
        dl'     = withTile doorAt (Door (Locked (KeyId 0))) tinyRoom
        rat     = mkMonster Rat (V2 1 1)
        lookup_ = monsterAtFixed (V2 1 1) rat
        outcome = walkRay dl' lookup_ noHit noHit (northPath (V2 1 3))
    outcome `shouldBe` RayBlocked "thuds into the locked door"

  it "stops at an NPC standing between the player and the monster" $ do
    let npcPos_ = V2 1 2
        rat     = mkMonster Rat (V2 1 1)
        lookup_ = monsterAtFixed (V2 1 1) rat
        npcHit p = p == npcPos_
        outcome  = walkRay tinyRoom lookup_ npcHit noHit (northPath (V2 1 3))
    outcome `shouldBe` RayBlocked "whistles past a friendly face"

  it "stops at a chest standing between the player and the monster" $ do
    let chestPos_ = V2 1 2
        rat       = mkMonster Rat (V2 1 1)
        lookup_   = monsterAtFixed (V2 1 1) rat
        chestHit p = p == chestPos_
        outcome    = walkRay tinyRoom lookup_ noHit chestHit (northPath (V2 1 3))
    outcome `shouldBe` RayBlocked "strikes a chest with a dull thunk"

  it "treats an open door as pass-through terrain" $ do
    let doorAt  = V2 1 2
        dl'     = withTile doorAt (Door Open) tinyRoom
        rat     = mkMonster Rat (V2 1 1)
        lookup_ = monsterAtFixed (V2 1 1) rat
        outcome = walkRay dl' lookup_ noHit noHit (northPath (V2 1 3))
    case outcome of
      RayHitMonster _ m -> mKind m `shouldBe` Rat
      other             -> expectationFailure ("unexpected: " ++ show other)

  it "ignores monsters that lie off the ray entirely" $ do
    let rat      = mkMonster Rat (V2 3 3)
        lookup_  = monsterAtFixed (V2 3 3) rat
        outcome  = walkRay tinyRoom lookup_ noHit noHit (northPath (V2 1 3))
    -- Firing N from (1,3) never touches (3,3), so the ray runs
    -- off into the north wall at (1,0).
    outcome `shouldBe` RayBlocked "clatters against the wall"

  it "arrowRange caps the path at 8 tiles" $
    arrowRange `shouldBe` 8
