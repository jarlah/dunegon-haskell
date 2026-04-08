module Main (main) where

import Brick
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import System.Random (newStdGen)

import Data.Char (ord)

import qualified Game.Audio as Audio
import Game.GameState
import Game.Input (handleKey)
import Game.Logic.Command (parseCommand)
import Game.Logic.Dungeon (defaultLevelConfig)
import Game.Logic.Quest (Quest(..), QuestStatus(..))
import Game.Render
  ( drawGame, bossAttr, fogAttr, npcAttr
  , saveMenuCursorAttr, saveMenuEmptyAttr
  , launchCursorAttr, launchDisabledAttr, launchTitleAttr
  )
import qualified Game.Save as Save
import Game.Save.Types (SaveMetadata(..))
import Game.Types (GameAction(..), Inventory(..))

-- | Build the Brick 'App' with audio closed into the event handler.
--   Passing 'Nothing' disables audio playback (silent run).
mkApp :: Maybe Audio.AudioSystem -> App GameState e ()
mkApp mAudio = App
  { appDraw         = drawGame
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent mAudio
  , appStartEvent   = pure ()
  , appAttrMap      = const $ attrMap V.defAttr
      [ (fogAttr,             fg V.brightBlack)
      , (npcAttr,             fg V.yellow)
      , (bossAttr,            fg V.red)
      , (saveMenuCursorAttr,  V.defAttr `V.withStyle` V.reverseVideo)
      , (saveMenuEmptyAttr,   fg V.brightBlack)
      , (launchCursorAttr,    V.defAttr `V.withStyle` V.reverseVideo)
      , (launchDisabledAttr,  fg V.brightBlack)
      , (launchTitleAttr,     fg V.brightYellow `V.withStyle` V.bold)
      ]
  }

handleEvent :: Maybe Audio.AudioSystem -> BrickEvent () e -> EventM () GameState ()
handleEvent mAudio (VtyEvent (V.EvKey key mods)) = do
  gs <- get
  case () of
    -- Launch screen swallows everything — the player is not yet
    -- "in the game", so no other modal or gameplay key should fire
    -- until they pick an entry point.
    _ | Just lm  <- gsLaunchMenu gs -> handleLaunchMenuKey lm key
      | gsConfirmQuit gs            -> handleConfirmQuitKey key
      | gsVictory gs                -> handleVictoryKey key
      | gsHelpOpen gs               -> handleHelpKey key
      | Just sm  <- gsSaveMenu gs   -> handleSaveMenuKey sm key
      | Just buf <- gsPrompt gs     -> handlePromptKey key buf
      | Just i   <- gsDialogue gs   -> handleDialogueKey mAudio i key
      | gsQuestLogOpen gs           -> handleQuestLogKey key
      | gsInventoryOpen gs          -> handleInventoryKey mAudio key
      | otherwise                   -> handleNormalKey mAudio key mods
  -- Every key event is an opportunity to swap music tracks — the
  -- player may have just walked into the boss room's line of sight,
  -- or descended onto the boss floor, or climbed back off it.
  updateMusicFor mAudio
handleEvent _ _ = pure ()

-- | Consult the current 'GameState' and tell the audio shell which
--   music loop should be playing. The decision is delegated to
--   'shouldPlayBossMusic', which keeps Main.hs free of dungeon
--   geometry and set imports.
updateMusicFor :: Maybe Audio.AudioSystem -> EventM () GameState ()
updateMusicFor Nothing      = pure ()
updateMusicFor (Just audio) = do
  gs <- get
  let track = if shouldPlayBossMusic gs
                then Audio.BossMusic
                else Audio.DungeonMusic
  liftIO $ Audio.setMusic audio track

-- | Keystrokes while the victory modal is shown. The game is over
--   but not dead — any of q / Q / Esc opens the quit-confirmation
--   modal (so a fat-fingered key can't misfire), and everything
--   else is swallowed. There's no "continue playing" path because
--   the dragon is dead and the boss floor has no stairs down.
handleVictoryKey :: V.Key -> EventM () GameState ()
handleVictoryKey (V.KChar 'q') =
  modify (\gs -> gs { gsConfirmQuit = True })
handleVictoryKey (V.KChar 'Q') =
  modify (\gs -> gs { gsConfirmQuit = True })
handleVictoryKey V.KEsc =
  modify (\gs -> gs { gsConfirmQuit = True })
handleVictoryKey _ = pure ()

-- | Keystrokes while the quit-confirmation modal is open.
--   @y@ actually halts the app; @n@, @Esc@, or anything else
--   closes the modal and returns to the game.
handleConfirmQuitKey :: V.Key -> EventM () GameState ()
handleConfirmQuitKey (V.KChar 'y') = halt
handleConfirmQuitKey (V.KChar 'Y') = halt
handleConfirmQuitKey _ =
  modify (\gs -> gs { gsConfirmQuit = False })

-- | Keystrokes while the help modal is open. Any key closes it —
--   there is no selection or sub-navigation, the modal is purely a
--   reference sheet.
handleHelpKey :: V.Key -> EventM () GameState ()
handleHelpKey _ =
  modify (\gs -> gs { gsHelpOpen = False })

-- | Keystrokes while the quest log modal is open. Letters @a@..@z@
--   select the active quest at that index (the selection is shown
--   as an asterisk in the log); pressing @x@ while a selection is
--   in place abandons that quest (flipping it to QuestFailed).
--   Two keystrokes = built-in confirm. Esc or @j@ closes.
handleQuestLogKey :: V.Key -> EventM () GameState ()
handleQuestLogKey V.KEsc =
  modify (\gs -> gs { gsQuestLogOpen = False, gsQuestLogCursor = Nothing })
handleQuestLogKey (V.KChar 'Q') =
  modify (\gs -> gs { gsQuestLogOpen = False, gsQuestLogCursor = Nothing })
handleQuestLogKey (V.KChar 'x') = do
  gs <- get
  case gsQuestLogCursor gs of
    Just idx -> modify (abandonQuest idx)
    Nothing  -> pure ()
handleQuestLogKey (V.KChar c)
  | c >= 'a' && c <= 'z' = do
      gs <- get
      let idx    = ord c - ord 'a'
          active = [ q | q <- gsQuests gs, qStatus q == QuestActive ]
      if idx < length active
        then modify (\s -> s { gsQuestLogCursor = Just idx })
        else pure ()
handleQuestLogKey _ = pure ()

-- | Keystrokes while an NPC dialogue modal is open. Lowercase
--   @a@..@z@ accept an offered quest; uppercase @A@..@Z@ hand in
--   a ready-to-turn-in quest the player is carrying; 'Esc' closes
--   the modal without any action. Monsters do not act either way
--   — peaceful conversation is a free action, as is collecting
--   quest rewards.
handleDialogueKey :: Maybe Audio.AudioSystem -> Int -> V.Key -> EventM () GameState ()
handleDialogueKey _ _ V.KEsc =
  modify (\gs -> gs { gsDialogue = Nothing })
handleDialogueKey mAudio npcIdx (V.KChar c)
  | c >= 'a' && c <= 'z' = do
      gs <- get
      let offerIdx = ord c - ord 'a'
          offers   = case drop npcIdx (gsNPCs gs) of
            (n : _) -> npcOffers n
            []      -> []
      if offerIdx < length offers
        then do
          modify (acceptQuestFromNPC npcIdx offerIdx)
          -- If that was the last offer *and* the player has no
          -- ready quests to turn in here, auto-close the dialogue
          -- so they don't stare at an empty quest list.
          autoCloseIfIdle npcIdx
        else pure ()
  | c >= 'A' && c <= 'Z' = do
      gs <- get
      let readyIdx = ord c - ord 'A'
          ready    = [ q | q <- gsQuests gs, qStatus q == QuestReadyToTurnIn ]
      if readyIdx < length ready
        then do
          modify (turnInQuest npcIdx readyIdx)
          playEventsFor mAudio
          autoCloseIfIdle npcIdx
        else pure ()
handleDialogueKey _ _ _ = pure ()

-- | Close the dialogue modal if the NPC has no remaining offers
-- /and/ the player has no further quests ready to hand in here.
-- Called after accept/turn-in so the player isn't left staring at
-- an empty dialogue screen.
autoCloseIfIdle :: Int -> EventM () GameState ()
autoCloseIfIdle npcIdx = do
  gs <- get
  let stillOffering = case drop npcIdx (gsNPCs gs) of
        (n : _) -> not (null (npcOffers n))
        []      -> False
      stillReady = any (\q -> qStatus q == QuestReadyToTurnIn) (gsQuests gs)
  if stillOffering || stillReady
    then pure ()
    else modify (\s -> s { gsDialogue = Nothing })

-- | Keystrokes while the inventory modal is open. Letters @a@..@z@
--   select the item at that index and apply its default action;
--   'Esc' or 'i' closes the modal without doing anything. Modal
--   actions still advance monsters via 'applyAction', so deciding
--   to swap gear mid-fight carries the usual tactical cost.
handleInventoryKey :: Maybe Audio.AudioSystem -> V.Key -> EventM () GameState ()
handleInventoryKey _ V.KEsc =
  modify (\gs -> gs { gsInventoryOpen = False })
handleInventoryKey _ (V.KChar 'i') =
  modify (\gs -> gs { gsInventoryOpen = False })
handleInventoryKey mAudio (V.KChar c)
  | c >= 'a' && c <= 'z' = do
      gs <- get
      let idx = ord c - ord 'a'
      if idx < length (invItems (gsInventory gs))
        then do
          modify (\s -> s { gsInventoryOpen = False })
          modify (applyAction (UseItem idx))
          playEventsFor mAudio
        else pure ()
handleInventoryKey _ _ = pure ()

-- | Keystrokes while the slash-command prompt is open. The prompt
--   swallows all input: 'Esc' cancels, 'Enter' submits and dispatches,
--   'Backspace' edits, printable characters append. Nothing else
--   advances the game.
handlePromptKey :: V.Key -> String -> EventM () GameState ()
handlePromptKey key buf = case key of
  V.KEsc ->
    modify (\gs -> gs { gsPrompt = Nothing })
  V.KEnter -> do
    modify (\gs -> gs { gsPrompt = Nothing })
    case parseCommand buf of
      Right cmd ->
        modify (applyCommand cmd)
      Left err ->
        modify (\gs -> gs { gsMessages = ("Error: " ++ err) : gsMessages gs })
  V.KBS ->
    modify (\gs -> gs { gsPrompt = Just (dropLast buf) })
  V.KChar c ->
    modify (\gs -> gs { gsPrompt = Just (buf ++ [c]) })
  _ ->
    pure ()
  where
    dropLast [] = []
    dropLast xs = init xs

-- | Keystrokes while the prompt is closed. @/@ opens the prompt;
--   @i@ opens the inventory modal; everything else goes through the
--   normal action keymap.
handleNormalKey :: Maybe Audio.AudioSystem -> V.Key -> [V.Modifier] -> EventM () GameState ()
handleNormalKey _ (V.KChar '/') _ =
  modify (\gs -> gs { gsPrompt = Just "" })
handleNormalKey _ (V.KChar '?') _ =
  modify (\gs -> gs { gsHelpOpen = True })
handleNormalKey _ (V.KChar 'i') _ =
  modify (\gs -> gs { gsInventoryOpen = True })
handleNormalKey _ (V.KChar 'Q') _ =
  modify (\gs -> gs { gsQuestLogOpen = True, gsQuestLogCursor = Nothing })
-- Quicksave (F5) and quickload (F9) are free actions: they do not
-- advance monsters and do not run through 'applyAction' — they
-- talk directly to the filesystem and report into 'gsMessages'.
handleNormalKey _ (V.KFun 5) _ = doQuicksave
handleNormalKey _ (V.KFun 9) _ = doQuickload
-- F2 / F3 open the full save and load picker modals respectively.
-- Both take a snapshot of the save directory at open time so the
-- entry list doesn't shift under the cursor mid-menu.
handleNormalKey _ (V.KFun 2) _ = openSaveMenu SaveMode
handleNormalKey _ (V.KFun 3) _ = openSaveMenu LoadMode
handleNormalKey mAudio key mods =
  case handleKey key mods of
    Just Quit ->
      -- Don't halt immediately — open a confirm modal. q and Q
      -- are one shift-key apart, so fat-fingering Quest Log
      -- would otherwise kill the run.
      modify (\gs -> gs { gsConfirmQuit = True })
    Just act  -> do
      modify (applyAction act)
      playEventsFor mAudio
    Nothing   -> pure ()

-- | Save the current 'GameState' to the quicksave slot and report
--   the outcome into the message log. Quicksave is a free action —
--   it does not emit game events, does not clear the event queue,
--   and does not advance monsters. On failure the game continues
--   untouched with an error line in the log so the player can see
--   what went wrong.
doQuicksave :: EventM () GameState ()
doQuicksave = do
  gs  <- get
  res <- liftIO (Save.writeSave Save.QuickSlot gs)
  let line = case res of
        Right ()                   -> "Quicksaved."
        Left Save.SaveMissing      -> "Quicksave failed: save directory missing."
        Left Save.SaveWrongMagic   -> "Quicksave failed: internal error (magic)."
        Left Save.SaveWrongVersion -> "Quicksave failed: internal error (version)."
        Left (Save.SaveCorrupt e)  -> "Quicksave failed: " ++ e
        Left (Save.SaveIOError e)  -> "Quicksave failed: " ++ e
  modify (\s -> s { gsMessages = line : gsMessages s })

-- | Replace the current 'GameState' with whatever is in the
--   quicksave slot. No monster advancement, no event emission —
--   the loaded state /is/ the new snapshot, modal flags included
--   (so quicksaving with the inventory open and quickloading
--   re-opens it). On failure the current state is preserved and
--   an error line is pushed into the message log.
doQuickload :: EventM () GameState ()
doQuickload = do
  res <- liftIO (Save.readSave Save.QuickSlot)
  case res of
    Right loaded ->
      -- The loaded state ships with its own message list; we prepend
      -- a breadcrumb so the player can see load happened without
      -- losing the saved history.
      put loaded { gsMessages = "Quickloaded." : gsMessages loaded }
    Left err -> do
      let line = case err of
            Save.SaveMissing       -> "No quicksave to load."
            Save.SaveWrongMagic    -> "Quicksave is not a valid save file."
            Save.SaveWrongVersion  -> "Quicksave is from an older version of the game."
            Save.SaveCorrupt e     -> "Quicksave is corrupted: " ++ e
            Save.SaveIOError e     -> "Quickload failed: " ++ e
      modify (\s -> s { gsMessages = line : gsMessages s })

-- | Fire SFX for every GameEvent the last action produced.
--   Extracted so the inventory and normal paths share one place
--   that knows how to route events into the audio shell.
playEventsFor :: Maybe Audio.AudioSystem -> EventM () GameState ()
playEventsFor Nothing      = pure ()
playEventsFor (Just audio) = do
  gs <- get
  liftIO $ mapM_ (Audio.playEvent audio) (gsEvents gs)

--------------------------------------------------------------------
-- Launch / title screen
--------------------------------------------------------------------

-- | Sample the save directory so the launch menu can enable or
--   disable /Continue/ and /Load/ without having to poll later.
--   Listing errors degrade to 'False' — the menu stays usable and
--   the player can still start a new game.
sampleHasSaves :: IO Bool
sampleHasSaves = do
  res <- Save.listSaves
  pure $ case res of
    Right (_:_) -> True
    _           -> False

-- | Keystrokes while the launch screen is up. Only navigation,
--   selection, and quit are meaningful — everything else is
--   swallowed so a fat-fingered key doesn't leak into the game.
handleLaunchMenuKey :: LaunchMenu -> V.Key -> EventM () GameState ()
handleLaunchMenuKey lm V.KUp =
  let n = length launchOptions
      c = (lmCursor lm - 1) `mod` n
  in modify $ \gs -> gs { gsLaunchMenu = Just lm { lmCursor = c } }
handleLaunchMenuKey lm V.KDown =
  let n = length launchOptions
      c = (lmCursor lm + 1) `mod` n
  in modify $ \gs -> gs { gsLaunchMenu = Just lm { lmCursor = c } }
handleLaunchMenuKey lm V.KEnter = runLaunchOption lm
handleLaunchMenuKey _  (V.KChar 'q') = halt
handleLaunchMenuKey _  (V.KChar 'Q') = halt
handleLaunchMenuKey _  V.KEsc        = halt
-- Letter shortcuts: n/c/l/q match the first letter of each option
-- so a keyboard player doesn't have to arrow-navigate.
handleLaunchMenuKey lm (V.KChar 'n') =
  runLaunchOption lm { lmCursor = 0 } -- New Game
handleLaunchMenuKey lm (V.KChar 'c')
  | lmHasSaves lm = runLaunchOption lm { lmCursor = 1 } -- Continue
handleLaunchMenuKey lm (V.KChar 'l')
  | lmHasSaves lm = runLaunchOption lm { lmCursor = 2 } -- Load
handleLaunchMenuKey _ _ = pure ()

-- | Execute the option currently highlighted by 'lmCursor'.
--   Disabled options (Continue / Load with no saves) are silently
--   ignored so the UI doesn't need a separate "error" state.
runLaunchOption :: LaunchMenu -> EventM () GameState ()
runLaunchOption lm = case drop (lmCursor lm) launchOptions of
  []        -> pure ()
  (opt : _) -> case opt of
    LaunchNewGame ->
      -- The state we started with is already a fresh run — just
      -- drop the launch menu and let the game take over.
      modify $ \gs -> gs { gsLaunchMenu = Nothing }
    LaunchContinue
      | not (lmHasSaves lm) -> pure ()
      | otherwise           -> loadMostRecent
    LaunchLoad
      | not (lmHasSaves lm) -> pure ()
      | otherwise           -> do
          -- Clear the launch menu first so the save picker layers
          -- on top of the gameplay screen the way it does mid-run.
          modify $ \gs -> gs { gsLaunchMenu = Nothing }
          openSaveMenu LoadMode
    LaunchQuit -> halt

-- | Load the most recently modified save, whatever slot it lives
--   in. 'Save.listSaves' already sorts newest-first, so we just
--   take the head. On any failure we surface a message and leave
--   the launch menu open so the player can pick again.
loadMostRecent :: EventM () GameState ()
loadMostRecent = do
  listRes <- liftIO Save.listSaves
  case listRes of
    Left err ->
      launchError ("Couldn't list saves: " ++ showSaveError err)
    Right []  ->
      launchError "No saves to continue from."
    Right (md : _) -> do
      readRes <- liftIO (Save.readSave (smSlot md))
      case readRes of
        Left err ->
          launchError ("Continue failed: " ++ showSaveError err)
        Right loaded ->
          -- Loaded state drops the launch menu automatically
          -- because 'performSaveAt' wiped it before the blob was
          -- written. Prepend a breadcrumb so the player sees the
          -- load happened in the message log.
          put loaded
            { gsLaunchMenu = Nothing
            , gsMessages   = "Continued from most recent save."
                             : gsMessages loaded
            }

-- | Push a one-line error into 'gsMessages' without closing the
--   launch menu. Used when 'Continue' can't find or can't read the
--   most recent save.
launchError :: String -> EventM () GameState ()
launchError msg =
  modify $ \gs -> gs { gsMessages = msg : gsMessages gs }

--------------------------------------------------------------------
-- Save / Load picker modal
--------------------------------------------------------------------

-- | Total number of numbered slots exposed in the UI. Keeping it
--   small on purpose — six slots is enough to run a few parallel
--   characters or keep a pre-boss backup, and more than that turns
--   into a chore to navigate.
numberedSlotCount :: Int
numberedSlotCount = 6

-- | Open the save/load modal in the given mode, snapshotting the
--   current save directory into 'smSlots'. If the listing itself
--   fails, fall back to a menu that only shows empty placeholder
--   rows — the player can still write into them, and the error
--   is surfaced in 'gsMessages' so the failure is visible.
openSaveMenu :: SaveMenuMode -> EventM () GameState ()
openSaveMenu mode = do
  res <- liftIO Save.listSaves
  let (metas, mErr) = case res of
        Right ms -> (ms, Nothing)
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
closeSaveMenu :: String -> EventM () GameState ()
closeSaveMenu msg =
  modify $ \gs -> gs
    { gsSaveMenu = Nothing
    , gsMessages = msg : gsMessages gs
    }

-- | Keystrokes while the save/load modal is open. The menu has
--   three layers: (1) normal cursor navigation + letter selection,
--   (2) overwrite confirmation (save mode only), and (3) close.
handleSaveMenuKey :: SaveMenu -> V.Key -> EventM () GameState ()
handleSaveMenuKey sm V.KEsc
  | smConfirm sm =
      -- Esc inside a confirm just cancels the confirm, not the menu.
      modify $ \gs -> gs { gsSaveMenu = Just sm { smConfirm = False } }
  | otherwise =
      modify $ \gs -> gs { gsSaveMenu = Nothing }
handleSaveMenuKey sm (V.KChar 'y')
  | smConfirm sm = performSaveAt sm (smCursor sm)
handleSaveMenuKey sm (V.KChar 'n')
  | smConfirm sm =
      modify $ \gs -> gs { gsSaveMenu = Just sm { smConfirm = False } }
handleSaveMenuKey sm (V.KChar c)
  | smConfirm sm = case c of
      -- 'Y' as a forgiving shift-variant.
      'Y' -> performSaveAt sm (smCursor sm)
      _   -> pure ()
  | c >= 'a' && c <= 'z' = do
      let idx = fromEnum c - fromEnum 'a'
      if idx < length (smSlots sm)
        then selectAt sm idx
        else pure ()
  | c == 'x' = deleteAt sm (smCursor sm)
handleSaveMenuKey _ V.KUp =
  modify $ \gs -> case gsSaveMenu gs of
    Just m ->
      gs { gsSaveMenu = Just m { smCursor = max 0 (smCursor m - 1) } }
    Nothing -> gs
handleSaveMenuKey _ V.KDown =
  modify $ \gs -> case gsSaveMenu gs of
    Just m ->
      let n = length (smSlots m)
          c = min (n - 1) (smCursor m + 1)
      in gs { gsSaveMenu = Just m { smCursor = c } }
    Nothing -> gs
handleSaveMenuKey _ V.KEnter =
  -- Enter on the cursor row is the same as pressing the row's
  -- letter key — acts as "select current". Routes through the
  -- same code path.
  do
    gs <- get
    case gsSaveMenu gs of
      Just m  -> selectAt m (smCursor m)
      Nothing -> pure ()
handleSaveMenuKey _ _ = pure ()

-- | Handle a slot row being picked via a letter or Enter.
--   Save mode: empty slot → write immediately; non-empty → ask to
--   confirm overwrite. Load mode: empty slot → no-op (can't load
--   nothing); non-empty → read and replace 'GameState'.
selectAt :: SaveMenu -> Int -> EventM () GameState ()
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
performSaveAt :: SaveMenu -> Int -> EventM () GameState ()
performSaveAt sm idx = case drop idx (smSlots sm) of
  []          -> pure ()
  (entry : _) -> do
    gs  <- get
    -- Wipe the menu from the state *before* writing so the saved
    -- blob doesn't ship with a stale menu snapshot baked in.
    let gsToSave = gs { gsSaveMenu = Nothing }
    res <- liftIO (Save.writeSave (entrySlot entry) gsToSave)
    case res of
      Right () ->
        closeSaveMenu ("Saved to " ++ sseSlotLabel entry ++ ".")
      Left err ->
        closeSaveMenu ("Save failed: " ++ showSaveError err)

-- | Commit a load from an existing slot and close the menu. On
--   success the loaded state wholesale replaces 'GameState' and
--   the menu field is cleared (it was 'Nothing' in the save file
--   because 'performSaveAt' wiped it before writing).
performLoadAt :: SaveMenuEntry -> EventM () GameState ()
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
deleteAt :: SaveMenu -> Int -> EventM () GameState ()
deleteAt sm idx = case drop idx (smSlots sm) of
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
                Right ms -> ms
                Left _   -> []
          modify $ \gs -> gs
            { gsSaveMenu = Just sm
                { smSlots   = buildSaveMenuEntries metas
                , smConfirm = False
                }
            , gsMessages = ("Deleted " ++ sseSlotLabel entry ++ ".") : gsMessages gs
            }

main :: IO ()
main = do
  gen <- newStdGen
  hasSaves <- sampleHasSaves
  -- The initial state is a fresh run with the launch screen
  -- layered on top. Picking /New Game/ drops the launch field and
  -- reveals this same state; picking /Continue/ or /Load/ replaces
  -- it wholesale with a deserialized 'GameState'. Sampling the
  -- save directory once at startup is enough because the menu
  -- stays open until the player commits to one path.
  let initialState = (newGame gen defaultLevelConfig)
        { gsLaunchMenu = Just LaunchMenu
            { lmCursor   = 0
            , lmHasSaves = hasSaves
            }
        }
      buildVty = VCP.mkVty V.defaultConfig
  -- Audio init is best-effort: if it fails (no device, missing
  -- assets, ...), 'bracket' still runs the game silently.
  bracket Audio.initAudio
          (mapM_ Audio.shutdownAudio)
          $ \mAudio -> do
    initialVty <- buildVty
    _ <- customMain initialVty buildVty Nothing (mkApp mAudio) initialState
    pure ()
