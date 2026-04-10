-- | Tests for the pure helpers in "Game.UI.SaveMenu". The 'EventM'
--   handlers are IO-bound and need a full filesystem sandbox to
--   exercise, so they're left out of this spec — the pure
--   row-building, slot-extraction, and error-rendering pieces are
--   the interesting ones to pin down in any case.
module Game.UI.SaveMenuSpec (spec) where

import Test.Hspec

import qualified Game.Save as Save
import Game.Save.Types (SaveMetadata (..), SaveSlot (..))
import Game.UI.SaveMenu
  ( buildSaveMenuEntries, entrySlot, filterLoadable
  , numberedSlotCount, showSaveError
  )
import Game.GameState (SaveMenuEntry (..))
import Game.UI.Types (RuntimeFlags (..))

-- | Build a 'SaveMetadata' fixture for a given slot. The scalar
--   fields are only used by the renderer (not by the helpers tested
--   here), so default values keep the call sites short.
mkMeta :: SaveSlot -> Bool -> SaveMetadata
mkMeta slot cheats = SaveMetadata
  { smSlot       = slot
  , smDepth      = 1
  , smPlayerLvl  = 1
  , smPlayerHP   = 20
  , smCheatsUsed = cheats
  }

wizard, clean :: RuntimeFlags
wizard = RuntimeFlags { rfWizardEnabled = True }
clean  = RuntimeFlags { rfWizardEnabled = False }

spec :: Spec
spec = do

  describe "filterLoadable" $ do

    it "passes every save through untouched in wizard mode" $ do
      let ms = [ mkMeta QuickSlot True
               , mkMeta (NumberedSlot 1) False
               , mkMeta (NumberedSlot 2) True
               ]
      filterLoadable wizard ms `shouldBe` ms

    it "drops cheat-tainted saves in clean mode" $ do
      let clean1 = mkMeta (NumberedSlot 1) False
          clean2 = mkMeta (NumberedSlot 3) False
          dirty  = mkMeta (NumberedSlot 2) True
      filterLoadable clean [clean1, dirty, clean2]
        `shouldBe` [clean1, clean2]

    it "is the identity on an empty list regardless of mode" $ do
      filterLoadable wizard [] `shouldBe` []
      filterLoadable clean  [] `shouldBe` []

    it "returns [] when every save is cheat-tainted and mode is clean" $ do
      let dirty = [mkMeta QuickSlot True, mkMeta (NumberedSlot 1) True]
      filterLoadable clean dirty `shouldBe` []

  describe "buildSaveMenuEntries" $ do

    it "produces quicksave plus exactly numberedSlotCount rows" $ do
      length (buildSaveMenuEntries [])
        `shouldBe` numberedSlotCount + 1

    it "renders every row as empty when the metadata list is empty" $ do
      map sseMeta (buildSaveMenuEntries [])
        `shouldBe` replicate (numberedSlotCount + 1) Nothing

    it "puts the quicksave row first" $ do
      case buildSaveMenuEntries [] of
        (e : _) -> sseIsQuick e `shouldBe` True
        []      -> expectationFailure "empty menu"

    it "fills the numbered rows in 1..N order" $ do
      let entries = buildSaveMenuEntries []
      map sseSlotNum (drop 1 entries)
        `shouldBe` [1 .. numberedSlotCount]

    it "attaches metadata to matching slots" $ do
      let q  = mkMeta QuickSlot False
          n2 = mkMeta (NumberedSlot 2) False
          entries = buildSaveMenuEntries [q, n2]
      case entries of
        (eq : _ : en2 : _) -> do
          sseMeta eq  `shouldBe` Just q
          sseMeta en2 `shouldBe` Just n2
        _ -> expectationFailure "unexpected entry shape"

    it "labels the quicksave row as 'Quick' and numbered rows as 'Slot N'" $ do
      let entries = buildSaveMenuEntries []
      map sseSlotLabel entries
        `shouldBe` ("Quick" : ["Slot " ++ show n | n <- [1 .. numberedSlotCount]])

  describe "entrySlot" $ do

    it "maps the quicksave row back to QuickSlot" $ do
      case buildSaveMenuEntries [] of
        (eq : _) -> entrySlot eq `shouldBe` QuickSlot
        []       -> expectationFailure "expected at least one entry"

    it "maps a numbered row back to its NumberedSlot" $ do
      let entries = buildSaveMenuEntries []
      case drop 3 entries of
        (e3 : _) -> entrySlot e3 `shouldBe` NumberedSlot 3
        []       -> expectationFailure "missing slot row"

  describe "showSaveError" $ do

    it "renders every SaveError constructor as a non-empty line" $ do
      showSaveError Save.SaveMissing       `shouldBe` "save file missing"
      showSaveError Save.SaveWrongMagic    `shouldBe` "file is not a valid save"
      showSaveError Save.SaveWrongVersion
        `shouldBe` "save is from an older version of the game"
      showSaveError (Save.SaveCorrupt "boom")
        `shouldBe` "save is corrupted: boom"
      showSaveError (Save.SaveIOError "EACCES") `shouldBe` "EACCES"
