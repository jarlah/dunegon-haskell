{-# LANGUAGE ScopedTypeVariables #-}
-- | Property + example tests for 'Game.Logic.FOV'.
--
--   The shaped fixtures are tiny hand-built rooms so we can reason
--   precisely about what must or must not be visible. The
--   QuickCheck properties use the same family of open rooms with
--   randomised viewer positions and radii.
module Game.Logic.FOVSpec (spec) where

import qualified Data.Set as Set
import qualified Data.Vector as V
import Linear (V2(..))
import Test.Hspec
import Test.QuickCheck

import Game.Logic.FOV (computeFOV, hasLineOfSight)
import Game.Types

-- | An open @w × h@ room with walls only on the outer edge. Floor
--   everywhere inside. Useful for "no obstructions" properties.
openRoom :: Int -> Int -> DungeonLevel
openRoom w h = DungeonLevel
  { dlWidth  = w
  , dlHeight = h
  , dlDepth  = 1
  , dlTiles  = V.generate (w * h) $ \i ->
      let (y, x) = i `divMod` w
      in if x == 0 || y == 0 || x == w - 1 || y == h - 1
           then Wall
           else Floor
  , dlRooms  = [Room 1 1 (w - 2) (h - 2)]
  }

-- | An open 11×11 room with a single wall at (5, 3). Used to
--   hand-verify that the wall blocks sight beyond it.
roomWithPillar :: DungeonLevel
roomWithPillar =
  let w = 11
      h = 11
      wallTile (V2 x y)
        | x == 0 || y == 0 || x == w - 1 || y == h - 1 = True
        | (x, y) == (5, 3)                             = True
        | otherwise                                    = False
  in DungeonLevel
       { dlWidth  = w
       , dlHeight = h
       , dlDepth  = 1
       , dlTiles  = V.generate (w * h) $ \i ->
           let (y, x) = i `divMod` w
           in if wallTile (V2 x y) then Wall else Floor
       , dlRooms  = [Room 1 1 (w - 2) (h - 2)]
       }

spec :: Spec
spec = describe "Game.Logic.FOV.computeFOV" $ do

  it "always includes the origin" $ property $ \(Positive r) ->
    forAll (choose (1, 8)) $ \innerW ->
    forAll (choose (1, 8)) $ \innerH ->
    forAll (choose (1, innerW)) $ \px ->
    forAll (choose (1, innerH)) $ \py ->
      let dl     = openRoom (innerW + 2) (innerH + 2)
          origin = V2 px py
          fov    = computeFOV dl origin (min r 10)
      in Set.member origin fov

  it "in an open room, adjacent floor tiles are all visible" $ do
    let dl     = openRoom 11 11
        origin = V2 5 5
        fov    = computeFOV dl origin 5
    let neighbors = [ V2 (5 + dx) (5 + dy)
                    | dx <- [-1 .. 1]
                    , dy <- [-1 .. 1]
                    , (dx, dy) /= (0, 0)
                    ]
    mapM_ (\p -> p `Set.member` fov `shouldBe` True) neighbors

  it "no visible tile lies beyond the requested radius" $ property $
    forAll (choose (1, 10)) $ \r ->
      let dl           = openRoom 21 21
          origin       = V2 10 10
          fov          = computeFOV dl origin r
          withinRadius (V2 x y) =
            let dx = x - 10
                dy = y - 10
            in dx * dx + dy * dy <= r * r
      in all withinRadius (Set.toList fov)

  it "is symmetric: if A sees B then B sees A" $ property $
    forAll (choose (1, 6)) $ \r ->
    forAll (choose (1, 9)) $ \ax ->
    forAll (choose (1, 9)) $ \ay ->
      let dl = openRoom 11 11
          a  = V2 ax ay
          fa = computeFOV dl a r
      in all (\b -> a `Set.member` computeFOV dl b r) (Set.toList fa)

  it "a wall blocks sight past it" $ do
    -- Viewer at (5, 5). Wall at (5, 3). Tile (5, 1) is directly
    -- behind the wall along a straight vertical line, so it MUST
    -- be blocked. Tile (5, 3) itself (the wall) is visible.
    let fov = computeFOV roomWithPillar (V2 5 5) 8
    V2 5 3 `Set.member` fov `shouldBe` True
    V2 5 1 `Set.member` fov `shouldBe` False

  it "never returns out-of-bounds positions" $ property $
    forAll (choose (1, 10)) $ \r ->
      let dl     = openRoom 11 11
          origin = V2 5 5
          fov    = computeFOV dl origin r
          inBounds (V2 x y) =
            x >= 0 && y >= 0 && x < dlWidth dl && y < dlHeight dl
      in all inBounds (Set.toList fov)

  describe "hasLineOfSight" $ do
    -- An 11x5 level with a single Door tile at (5,2). The door
    -- state is parameterised so each test can pick open vs closed.
    -- Everything else on row 2 is floor, so a horizontal line
    -- from the west half to the east half must pass through the
    -- door — closed blocks it, open admits it.
    let corridorWith doorTile = DungeonLevel
          { dlWidth  = 11
          , dlHeight = 5
          , dlDepth  = 1
          , dlTiles  = V.generate (11 * 5) $ \i ->
              let (y, x) = i `divMod` 11
              in if y == 0 || y == 4 || x == 0 || x == 10
                   then Wall
                   else if y == 2 && x == 5
                     then doorTile
                     else Floor
          , dlRooms  = [Room 1 1 9 3]
          }

    it "is clear across an open room" $ do
      let dl = openRoom 11 11
      hasLineOfSight dl (V2 1 1) (V2 9 9) `shouldBe` True

    it "is blocked by an intervening wall" $ do
      -- roomWithPillar has a wall at (5,3). A line from (5,5) to
      -- (5,1) passes through (5,3), so LOS is blocked.
      hasLineOfSight roomWithPillar (V2 5 5) (V2 5 1) `shouldBe` False

    it "is blocked by a closed door on the line" $
      hasLineOfSight (corridorWith (Door Closed)) (V2 2 2) (V2 8 2)
        `shouldBe` False

    it "is clear through an open door on the line" $
      hasLineOfSight (corridorWith (Door Open)) (V2 2 2) (V2 8 2)
        `shouldBe` True

    it "is symmetric" $ property $
      forAll (choose (1, 9)) $ \ax ->
      forAll (choose (1, 9)) $ \ay ->
      forAll (choose (1, 9)) $ \bx ->
      forAll (choose (1, 9)) $ \by ->
        let dl = roomWithPillar
            a  = V2 ax ay
            b  = V2 bx by
        in hasLineOfSight dl a b === hasLineOfSight dl b a
