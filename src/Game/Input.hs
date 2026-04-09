module Game.Input (handleKey) where

import qualified Graphics.Vty as V

import Game.Types

-- | Translate a Vty key press into a 'GameAction', if any.
--
--   Shift + arrow (and Shift + hjkl / yubn) maps to 'Dash', the
--   cooldown-gated escape dash. Plain arrows / vi keys stay on
--   single-step 'Move'. Upper-case @H@ / @J@ etc. are treated as
--   shifted vi keys and also dash; the vty layer reports them as
--   @KChar 'H'@ with no modifier, which is why we dispatch on the
--   character itself rather than on the modifier list for those.
handleKey :: V.Key -> [V.Modifier] -> Maybe GameAction
handleKey key mods =
  let shifted = V.MShift `elem` mods
  in case key of
    V.KUp    | shifted -> Just (Dash N)
    V.KDown  | shifted -> Just (Dash S)
    V.KLeft  | shifted -> Just (Dash W)
    V.KRight | shifted -> Just (Dash E)
    V.KUp       -> Just (Move N)
    V.KDown     -> Just (Move S)
    V.KLeft     -> Just (Move W)
    V.KRight    -> Just (Move E)
    V.KChar 'k' -> Just (Move N)
    V.KChar 'j' -> Just (Move S)
    V.KChar 'h' -> Just (Move W)
    V.KChar 'l' -> Just (Move E)
    V.KChar 'y' -> Just (Move NW)
    V.KChar 'u' -> Just (Move NE)
    V.KChar 'b' -> Just (Move SW)
    V.KChar 'n' -> Just (Move SE)
    V.KChar 'K' -> Just (Dash N)
    V.KChar 'J' -> Just (Dash S)
    V.KChar 'H' -> Just (Dash W)
    V.KChar 'L' -> Just (Dash E)
    V.KChar 'Y' -> Just (Dash NW)
    V.KChar 'U' -> Just (Dash NE)
    V.KChar 'B' -> Just (Dash SW)
    V.KChar 'N' -> Just (Dash SE)
    V.KChar '.' -> Just Wait
    V.KChar 'g' -> Just Pickup
    V.KChar '>' -> Just GoDownStairs
    V.KChar '<' -> Just GoUpStairs
    V.KChar 'q' -> Just Quit
    V.KEsc      -> Just Quit
    _           -> Nothing
