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
import Game.Render (drawGame, bossAttr, fogAttr, npcAttr)
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
      [ (fogAttr,  fg V.brightBlack)
      , (npcAttr,  fg V.yellow)
      , (bossAttr, fg V.red)
      ]
  }

handleEvent :: Maybe Audio.AudioSystem -> BrickEvent () e -> EventM () GameState ()
handleEvent mAudio (VtyEvent (V.EvKey key mods)) = do
  gs <- get
  case () of
    _ | gsConfirmQuit gs            -> handleConfirmQuitKey key
      | gsVictory gs                -> handleVictoryKey key
      | gsHelpOpen gs               -> handleHelpKey key
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

-- | Fire SFX for every GameEvent the last action produced.
--   Extracted so the inventory and normal paths share one place
--   that knows how to route events into the audio shell.
playEventsFor :: Maybe Audio.AudioSystem -> EventM () GameState ()
playEventsFor Nothing      = pure ()
playEventsFor (Just audio) = do
  gs <- get
  liftIO $ mapM_ (Audio.playEvent audio) (gsEvents gs)

main :: IO ()
main = do
  gen <- newStdGen
  let initialState = newGame gen defaultLevelConfig
      buildVty     = VCP.mkVty V.defaultConfig
  -- Audio init is best-effort: if it fails (no device, missing
  -- assets, ...), 'bracket' still runs the game silently.
  bracket Audio.initAudio
          (mapM_ Audio.shutdownAudio)
          $ \mAudio -> do
    initialVty <- buildVty
    _ <- customMain initialVty buildVty Nothing (mkApp mAudio) initialState
    pure ()
