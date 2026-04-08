module Main (main) where

import Brick
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import System.Random (newStdGen)

import qualified Game.Audio as Audio
import Game.GameState
import Game.Input (handleKey)
import Game.Logic.Command (parseCommand)
import Game.Logic.Dungeon (defaultLevelConfig)
import Game.Render (drawGame, fogAttr)
import Game.Types (GameAction(..))

-- | Build the Brick 'App' with audio closed into the event handler.
--   Passing 'Nothing' disables audio playback (silent run).
mkApp :: Maybe Audio.AudioSystem -> App GameState e ()
mkApp mAudio = App
  { appDraw         = drawGame
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent mAudio
  , appStartEvent   = pure ()
  , appAttrMap      = const $ attrMap V.defAttr
      [ (fogAttr, fg V.brightBlack)
      ]
  }

handleEvent :: Maybe Audio.AudioSystem -> BrickEvent () e -> EventM () GameState ()
handleEvent mAudio (VtyEvent (V.EvKey key mods)) = do
  gs <- get
  case gsPrompt gs of
    Just buf -> handlePromptKey key buf
    Nothing  -> handleNormalKey mAudio key mods
handleEvent _ _ = pure ()

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
--   everything else goes through the normal action keymap.
handleNormalKey :: Maybe Audio.AudioSystem -> V.Key -> [V.Modifier] -> EventM () GameState ()
handleNormalKey _ (V.KChar '/') _ =
  modify (\gs -> gs { gsPrompt = Just "" })
handleNormalKey mAudio key mods =
  case handleKey key mods of
    Just Quit -> halt
    Just act  -> do
      modify (applyAction act)
      case mAudio of
        Nothing    -> pure ()
        Just audio -> do
          gs <- get
          liftIO $ mapM_ (Audio.playEvent audio) (gsEvents gs)
    Nothing   -> pure ()

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
