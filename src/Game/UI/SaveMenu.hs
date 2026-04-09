-- | Save / load picker modal machinery, extracted from
--   @app/Main.hs@. The module owns both the pure helpers that shape
--   a 'SaveMenu' from a save-directory listing and the 'EventM'
--   handlers that drive the modal itself. Keeping the save-menu
--   code in its own module lets Main.hs stay focused on the Brick
--   'App' wiring, and makes the pure pieces reachable from the
--   test suite.
module Game.UI.SaveMenu
  ( -- * Helpers shared with the launch screen
    filterLoadable
  , sampleHasSaves
    -- * Configuration
  , numberedSlotCount
    -- * Pure menu construction
  , buildSaveMenuEntries
  , entrySlot
  , showSaveError
    -- * EventM handlers
  , openSaveMenu
  , closeSaveMenu
  , handleSaveMenuKey
  , selectAt
  , performSaveAt
  , performLoadAt
  , deleteAt
  ) where

import Brick (EventM, get, modify, put)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V

import Game.GameState
import qualified Game.Save as Save
import Game.Save.Types (SaveMetadata (..))
import Game.UI.Types (Name, RuntimeFlags (..))

-- | Drop cheat-tainted saves from a metadata list when the process
--   is not in wizard mode. In wizard mode the list passes through
--   untouched. The launch menu, load picker, and @Continue@ all
--   funnel their listings through this helper so the filter
--   policy stays in one place.
filterLoadable :: RuntimeFlags -> [SaveMetadata] -> [SaveMetadata]
filterLoadable rFlags
  | rfWizardEnabled rFlags = id
  | otherwise              = filter (not . smCheatsUsed)

-- | Sample the save directory so the launch menu can enable or
--   disable /Continue/ and /Load/ without having to poll later.
--   Applies the wizard-mode filter so a non-wizard session with
--   only cheat-tainted saves still sees the options greyed out.
--   Listing errors degrade to 'False' — the menu stays usable and
--   the player can still start a new game.
sampleHasSaves :: RuntimeFlags -> IO Bool
sampleHasSaves rFlags = do
  res <- Save.listSaves
  pure $ case res of
    Right ms -> not (null (filterLoadable rFlags ms))
    _        -> False

-- | Total number of numbered slots exposed in the UI. Keeping it
--   small on purpose — six slots is enough to run a few parallel
--   characters or keep a pre-boss backup, and more than that turns
--   into a chore to navigate.
numberedSlotCount :: Int
numberedSlotCount = 6

-- | Open the save/load modal in the given mode, snapshotting the
--   current save directory into 'smSlots'. Cheat-tainted saves
--   are filtered out via 'filterLoadable' so a non-wizard session
--   doesn't see them in either mode (writing over one reclaims
--   the slot as a clean save, reading one is blocked). If the
--   listing itself fails, fall back to a menu that only shows
--   empty placeholder rows — the player can still write into
--   them, and the error is surfaced in 'gsMessages' so the
--   failure is visible.
openSaveMenu :: RuntimeFlags -> SaveMenuMode -> EventM Name GameState ()
openSaveMenu rFlags mode = do
  res <- liftIO Save.listSaves
  let (metas, mErr) = case res of
        Right ms -> (filterLoadable rFlags ms, Nothing)
        Left err -> ([], Just err)
      entries = buildSaveMenuEntries metas
      menu    = SaveMenu
        { smMode    = mode
        , smSlots   = entries
        , smCursor  = 0
        , smConfirm = False
        }
  modify $ \gs -> gs
    { gsSaveMenu = Just menu
    , gsMessages = case mErr of
        Just err -> ("Couldn't list saves: " ++ showSaveError err) : gsMessages gs
        Nothing  -> gsMessages gs
    }

-- | Build the fixed row order for the save picker: the quicksave
--   first, then the numbered slots 1..'numberedSlotCount'. Each row
--   is matched against the live listing so existing saves get their
--   metadata attached and empty slots render as placeholders.
buildSaveMenuEntries :: [SaveMetadata] -> [SaveMenuEntry]
buildSaveMenuEntries metas =
  let findMeta slot = [ m | m <- metas, smSlot m == slot ]
      quickEntry = SaveMenuEntry
        { sseMeta      = listToMaybeHead (findMeta Save.QuickSlot)
        , sseSlotLabel = "Quick"
        , sseIsQuick   = True
        , sseSlotNum   = 0
        }
      numbered n = SaveMenuEntry
        { sseMeta      = listToMaybeHead (findMeta (Save.NumberedSlot n))
        , sseSlotLabel = "Slot " ++ show n
        , sseIsQuick   = False
        , sseSlotNum   = n
        }
  in quickEntry : [ numbered n | n <- [1 .. numberedSlotCount] ]
  where
    listToMaybeHead []      = Nothing
    listToMaybeHead (x : _) = Just x

-- | Convert a 'SaveMenuEntry' back into the concrete 'Save.SaveSlot'
--   its row represents. Used when a letter key selects a slot to
--   read / write / delete.
entrySlot :: SaveMenuEntry -> Save.SaveSlot
entrySlot e
  | sseIsQuick e = Save.QuickSlot
  | otherwise    = Save.NumberedSlot (sseSlotNum e)

-- | Render a 'Save.SaveError' as a single line of user-facing text.
showSaveError :: Save.SaveError -> String
showSaveError err = case err of
  Save.SaveMissing       -> "save file missing"
  Save.SaveWrongMagic    -> "file is not a valid save"
  Save.SaveWrongVersion  -> "save is from an older version of the game"
  Save.SaveCorrupt e     -> "save is corrupted: " ++ e
  Save.SaveIOError e     -> e

-- | Close the save menu and append a message to the log.
closeSaveMenu :: String -> EventM Name GameState ()
closeSaveMenu msg =
  modify $ \gs -> gs
    { gsSaveMenu = Nothing
    , gsMessages = msg : gsMessages gs
    }

-- | Keystrokes while the save/load modal is open. The menu has
--   three layers: (1) normal cursor navigation + letter selection,
--   (2) overwrite confirmation (save mode only), and (3) close.
handleSaveMenuKey :: RuntimeFlags -> SaveMenu -> V.Key -> EventM Name GameState ()
handleSaveMenuKey _ sm V.KEsc
  | smConfirm sm =
      -- Esc inside a confirm just cancels the confirm, not the menu.
      modify $ \gs -> gs { gsSaveMenu = Just sm { smConfirm = False } }
  | otherwise =
      modify $ \gs -> gs { gsSaveMenu = Nothing }
handleSaveMenuKey _ sm (V.KChar 'y')
  | smConfirm sm = performSaveAt sm (smCursor sm)
handleSaveMenuKey _ sm (V.KChar 'n')
  | smConfirm sm =
      modify $ \gs -> gs { gsSaveMenu = Just sm { smConfirm = False } }
handleSaveMenuKey rFlags sm (V.KChar c)
  | smConfirm sm = case c of
      -- 'Y' as a forgiving shift-variant.
      'Y' -> performSaveAt sm (smCursor sm)
      _   -> pure ()
  | c >= 'a' && c <= 'z' = do
      let idx = fromEnum c - fromEnum 'a'
      if idx < length (smSlots sm)
        then selectAt sm idx
        else pure ()
  | c == 'x' = deleteAt rFlags sm (smCursor sm)
handleSaveMenuKey _ _ V.KUp =
  modify $ \gs -> case gsSaveMenu gs of
    Just m ->
      gs { gsSaveMenu = Just m { smCursor = max 0 (smCursor m - 1) } }
    Nothing -> gs
handleSaveMenuKey _ _ V.KDown =
  modify $ \gs -> case gsSaveMenu gs of
    Just m ->
      let n = length (smSlots m)
          c = min (n - 1) (smCursor m + 1)
      in gs { gsSaveMenu = Just m { smCursor = c } }
    Nothing -> gs
handleSaveMenuKey _ _ V.KEnter =
  -- Enter on the cursor row is the same as pressing the row's
  -- letter key — acts as "select current". Routes through the
  -- same code path.
  do
    gs <- get
    case gsSaveMenu gs of
      Just m  -> selectAt m (smCursor m)
      Nothing -> pure ()
handleSaveMenuKey _ _ _ = pure ()

-- | Handle a slot row being picked via a letter or Enter.
--   Save mode: empty slot → write immediately; non-empty → ask to
--   confirm overwrite. Load mode: empty slot → no-op (can't load
--   nothing); non-empty → read and replace 'GameState'.
selectAt :: SaveMenu -> Int -> EventM Name GameState ()
selectAt sm idx = case drop idx (smSlots sm) of
  []          -> pure ()
  (entry : _) -> case smMode sm of
    SaveMode -> case sseMeta entry of
      Nothing ->
        -- Empty slot — write directly without confirm.
        performSaveAt (sm { smCursor = idx }) idx
      Just _  ->
        modify $ \gs -> gs
          { gsSaveMenu = Just sm { smCursor = idx, smConfirm = True } }
    LoadMode -> case sseMeta entry of
      Nothing -> pure ()      -- empty, nothing to load
      Just _  -> performLoadAt entry

-- | Commit a save to the slot at the given index and close the
--   menu with a status message. Uses the cursor from the passed-in
--   menu state so confirm-then-yes targets the row the user saw.
performSaveAt :: SaveMenu -> Int -> EventM Name GameState ()
performSaveAt sm idx = case drop idx (smSlots sm) of
  []          -> pure ()
  (entry : _) -> do
    gs  <- get
    -- Wipe the menu from the state *before* writing so the saved
    -- blob doesn't ship with a stale menu snapshot baked in, and
    -- bump the run-stats save counter so the persisted blob
    -- already reflects this save (loading it back gives the same
    -- counter as "kept running"). Commit the bump to the live
    -- state only on success, so a failed save doesn't nudge the
    -- rank.
    let gsToSave = gs { gsSaveMenu   = Nothing
                      , gsSavesUsed  = gsSavesUsed gs + 1
                      }
    res <- liftIO (Save.writeSave (entrySlot entry) gsToSave)
    case res of
      Right () -> do
        put gsToSave
        closeSaveMenu ("Saved to " ++ sseSlotLabel entry ++ ".")
      Left err ->
        closeSaveMenu ("Save failed: " ++ showSaveError err)

-- | Commit a load from an existing slot and close the menu. On
--   success the loaded state wholesale replaces 'GameState' and
--   the menu field is cleared (it was 'Nothing' in the save file
--   because 'performSaveAt' wiped it before writing).
performLoadAt :: SaveMenuEntry -> EventM Name GameState ()
performLoadAt entry = do
  res <- liftIO (Save.readSave (entrySlot entry))
  case res of
    Right loaded ->
      put loaded
        { gsSaveMenu = Nothing
        , gsMessages = ("Loaded " ++ sseSlotLabel entry ++ ".") : gsMessages loaded
        }
    Left err ->
      closeSaveMenu ("Load failed: " ++ showSaveError err)

-- | Delete the save at the cursor row and refresh the menu in
--   place. Idempotent — 'Save.deleteSave' succeeds silently when
--   the file is already absent.
deleteAt :: RuntimeFlags -> SaveMenu -> Int -> EventM Name GameState ()
deleteAt rFlags sm idx = case drop idx (smSlots sm) of
  []          -> pure ()
  (entry : _) -> case sseMeta entry of
    Nothing -> pure ()   -- nothing to delete
    Just _  -> do
      res <- liftIO (Save.deleteSave (entrySlot entry))
      case res of
        Left err ->
          closeSaveMenu ("Delete failed: " ++ showSaveError err)
        Right () -> do
          -- Re-list so the row flips to 'empty'. Cheap at our sizes.
          listRes <- liftIO Save.listSaves
          let metas = case listRes of
                Right ms -> filterLoadable rFlags ms
                Left _   -> []
          modify $ \gs -> gs
            { gsSaveMenu = Just sm
                { smSlots   = buildSaveMenuEntries metas
                , smConfirm = False
                }
            , gsMessages = ("Deleted " ++ sseSlotLabel entry ++ ".") : gsMessages gs
            }
