-- | Slash-command prompt and command dispatch, extracted from
--   @app/Main.hs@.
--
--   This module owns two things:
--
--   * 'handlePromptKey' / 'dispatchCommand' — the small state
--     machine that eats keystrokes while the @/command@ prompt is
--     open, parses the buffer on Enter, and routes the resulting
--     'Command' to the right effect.
--   * 'doQuicksave' / 'doQuickload' — the free-action quick-slot
--     helpers. They live here (rather than in the normal-mode
--     handler) because 'dispatchCommand' needs to call them for
--     the @/quicksave@ and @/quickload@ commands, and the
--     normal-mode @F5@ / @F9@ bindings in "Main" can import them
--     from the same place.
module Game.UI.Prompt
  ( handlePromptKey
  , dispatchCommand
  , doQuicksave
  , doQuickload
  ) where

import Brick (EventM, get, modify, put)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V

import qualified Game.Audio as Audio
import Game.GameState
import Game.Logic.Command (Command (..), isCheatCommand, parseCommand)
import qualified Game.Save as Save
import Game.Types (GameAction (..))
import Game.UI.Modals (playEventsFor)
import Game.UI.SaveMenu (openSaveMenu)
import Game.UI.Types (Name, RuntimeFlags (..))

-- | Keystrokes while the slash-command prompt is open. The prompt
--   swallows all input: 'Esc' cancels, 'Enter' submits and
--   dispatches, 'Backspace' edits, printable characters append.
--   Nothing else advances the game.
--
--   Dispatch splits two ways. Safe UI commands (@/help@, @/save@,
--   @/wait@, ...) are handled inline because several of them need
--   to open modals or touch the filesystem. Wizard / cheat
--   commands go through 'applyCommand' and are refused unless the
--   game was launched with @--wizard@ (surfaced in 'rFlags').
handlePromptKey
  :: Maybe Audio.AudioSystem
  -> RuntimeFlags
  -> V.Key
  -> String
  -> EventM Name GameState ()
handlePromptKey mAudio rFlags key buf = case key of
  V.KEsc ->
    modify (\gs -> gs { gsPrompt = Nothing })
  V.KEnter -> do
    modify (\gs -> gs { gsPrompt = Nothing })
    case parseCommand buf of
      Right cmd -> dispatchCommand mAudio rFlags cmd
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

-- | Route a parsed 'Command' to the right handler. Safe UI
--   commands open modals or call the filesystem helpers inline;
--   wizard cheats flow through 'applyCommand' after a capability
--   check against 'rfWizardEnabled'.
dispatchCommand
  :: Maybe Audio.AudioSystem
  -> RuntimeFlags
  -> Command
  -> EventM Name GameState ()
dispatchCommand mAudio rFlags cmd = case cmd of
  -- Safe UI shortcuts: these are alternate paths to things the
  -- player could already trigger with a keybind. No cheat check.
  CmdHelp ->
    modify (\gs -> gs { gsHelpOpen = True })
  CmdQuit ->
    modify (\gs -> gs { gsConfirmQuit = True })
  CmdInventory ->
    modify (\gs -> gs { gsInventoryOpen = True })
  CmdQuests ->
    modify $ \gs -> gs
      { gsQuestLogOpen   = True
      , gsQuestLogCursor = Nothing
      }
  CmdWait -> do
    modify (applyAction Wait)
    playEventsFor mAudio
  CmdSave       -> openSaveMenu rFlags SaveMode
  CmdLoad       -> openSaveMenu rFlags LoadMode
  CmdQuicksave  -> doQuicksave
  CmdQuickload  -> doQuickload
  -- Wizard cheats: gated on the runtime capability flag.
  _
    | isCheatCommand cmd, not (rfWizardEnabled rFlags) ->
        modify $ \gs -> gs
          { gsMessages =
              "Cheats are disabled. Launch with --wizard to enable."
                : gsMessages gs
          }
    | otherwise ->
        modify (applyCommand cmd)

-- | Save the current 'GameState' to the quicksave slot and report
--   the outcome into the message log. Quicksave is a free action —
--   it does not emit game events, does not clear the event queue,
--   and does not advance monsters. On failure the game continues
--   untouched with an error line in the log so the player can see
--   what went wrong.
doQuicksave :: EventM Name GameState ()
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
doQuickload :: EventM Name GameState ()
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
