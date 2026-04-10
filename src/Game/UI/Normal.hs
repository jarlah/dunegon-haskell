-- | Normal-mode key handling, extracted from @app/Main.hs@.
--
--   This is the handler that runs when no modal is on screen: the
--   player is walking the dungeon, and each keystroke either opens
--   a modal (inventory, help, prompt, save menu), kicks off a free
--   action (quicksave / quickload), or goes through 'handleKey' to
--   produce a 'GameAction' that advances the world.
module Game.UI.Normal
  ( handleNormalKey
  ) where

import Brick (EventM, modify)
import qualified Graphics.Vty as V

import Game.AI.Runtime (AIRuntime)
import qualified Game.Audio as Audio
import Game.Core
import Game.Input (handleKey)
import Game.Types (GameAction (..))
import Game.UI.Modals (playEventsFor)
import Game.UI.Prompt (doQuickload, doQuicksave)
import Game.UI.SaveMenu (openSaveMenu)
import Game.UI.Types (Name, RuntimeFlags)

-- | Keystrokes while the prompt is closed. @/@ opens the prompt;
--   @i@ opens the inventory modal; everything else goes through the
--   normal action keymap.
handleNormalKey
  :: Maybe Audio.AudioSystem
  -> AIRuntime
  -> RuntimeFlags
  -> V.Key
  -> [V.Modifier]
  -> EventM Name GameState ()
handleNormalKey _ _ _ V.KEsc _ =
  modify (\s -> s { gsConfirmQuit = True })
handleNormalKey _ _ _ (V.KChar '/') _ =
  modify (\gs -> gs { gsPrompt = Just "" })
handleNormalKey _ _ _ (V.KChar '?') _ =
  modify (\gs -> gs { gsHelpOpen = True })
handleNormalKey _ _ _ (V.KChar 'i') _ =
  modify (\gs -> gs { gsInventoryOpen = True })
-- 'c' enters the two-step close-door mode: prompt for a direction
-- key, then dispatch 'CloseDoor' on the next keystroke. No turn is
-- consumed here — the turn is spent (or not) when the direction
-- actually resolves in 'handleAwaitingDirectionKey'.
handleNormalKey _ _ _ (V.KChar 'c') _ =
  modify $ \gs -> gs
    { gsAwaitingDirection = Just DirCloseDoor
    , gsMessages = "Close door in which direction?" : gsMessages gs
    }
-- 'f' mirrors 'c': prompt for a direction key and dispatch 'Fire'
-- on the next keystroke. Precondition errors (no bow / no arrows)
-- are surfaced by 'fireArrow' itself, and the turn only advances
-- on a successful shot.
handleNormalKey _ _ _ (V.KChar 'f') _ =
  modify $ \gs -> gs
    { gsAwaitingDirection = Just DirFire
    , gsMessages = "Fire in which direction?" : gsMessages gs
    }
handleNormalKey _ _ _ (V.KChar 'Q') _ =
  modify (\gs -> gs { gsQuestLogOpen = True, gsQuestLogCursor = Nothing })
-- Quicksave (F5) and quickload (F9) are free actions: they do not
-- advance monsters and do not run through 'applyAction' — they
-- talk directly to the filesystem and report into 'gsMessages'.
handleNormalKey _ _ _ (V.KFun 5) _ = doQuicksave
handleNormalKey _ _ _ (V.KFun 9) _ = doQuickload
-- F2 / F3 open the full save and load picker modals respectively.
-- Both take a snapshot of the save directory at open time so the
-- entry list doesn't shift under the cursor mid-menu.
handleNormalKey _ _ rFlags (V.KFun 2) _ = openSaveMenu rFlags SaveMode
handleNormalKey _ _ rFlags (V.KFun 3) _ = openSaveMenu rFlags LoadMode
handleNormalKey mAudio _ _ key mods =
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
