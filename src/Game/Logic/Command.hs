-- | Pure parser for the slash-command prompt.
--
--   The prompt layer (render + input) lives in 'Main' / 'Render';
--   everything reachable through the prompt is defined here as a
--   plain ADT and a pure @parseCommand@ function so it can be
--   unit-tested without touching Brick.
--
--   Every command is a *wizard / debug* helper right now. They
--   don't cost a turn and they don't advance monsters — they are
--   tools for poking at game state while developing or
--   playtesting. Gameplay commands (@/talk@, @/pray@, ...) would
--   route differently.
module Game.Logic.Command
  ( Command(..)
  , parseCommand
  ) where

import Data.Char (toLower)
import Linear (V2(..))
import Text.Read (readMaybe)

import Game.Types (MonsterKind(..), Pos)

-- | A parsed prompt command. Adding a new command is a matter of
--   adding a constructor here, a parser case in 'parseCommand',
--   and a dispatch arm in 'Game.GameState.applyCommand'.
data Command
  = CmdReveal
    -- ^ mark every tile on the current level as explored
  | CmdHeal
    -- ^ restore the player to full HP
  | CmdKillAll
    -- ^ clear every monster from the current level
  | CmdTeleport !Pos
    -- ^ move the player to an arbitrary walkable tile
  | CmdSpawn !MonsterKind
    -- ^ drop a monster adjacent to the player
  | CmdXP !Int
    -- ^ grant N experience points (pairs with the level curve)
  | CmdDescend
    -- ^ force-descend without standing on stairs
  | CmdAscend
    -- ^ force-ascend without standing on stairs (refused at depth 1)
  deriving (Eq, Show)

-- | Parse a raw prompt buffer into a 'Command'. Leading whitespace
--   and a single leading @/@ are tolerated so the user can type
--   either @reveal@ or @/reveal@. The verb is matched
--   case-insensitively; arguments keep their case (but the only
--   arguments we currently parse are integers and monster kinds,
--   which are matched case-insensitively as well).
parseCommand :: String -> Either String Command
parseCommand raw =
  case words (stripLeadSlash (dropWhile (== ' ') raw)) of
    [] -> Left "empty command"
    (w : args) -> case map toLower w of
      "reveal"    -> nullary CmdReveal args
      "heal"      -> nullary CmdHeal args
      "kill-all"  -> nullary CmdKillAll args
      "killall"   -> nullary CmdKillAll args
      "descend"   -> nullary CmdDescend args
      "ascend"    -> nullary CmdAscend args
      "tp"        -> parseTeleport args
      "teleport"  -> parseTeleport args
      "spawn"     -> parseSpawn args
      "xp"        -> parseXP args
      other       -> Left ("unknown command: " ++ other)
  where
    stripLeadSlash ('/' : rest) = rest
    stripLeadSlash s            = s

-- | Helper for commands that take no arguments; errors out if the
--   user typed extra tokens so mistakes are visible.
nullary :: Command -> [String] -> Either String Command
nullary c [] = Right c
nullary _ xs = Left ("command takes no arguments, got: " ++ unwords xs)

parseTeleport :: [String] -> Either String Command
parseTeleport [xs, ys] = case (readMaybe xs, readMaybe ys) of
  (Just x, Just y) -> Right (CmdTeleport (V2 x y))
  _                -> Left "usage: tp X Y (X and Y must be integers)"
parseTeleport _ = Left "usage: tp X Y"

parseSpawn :: [String] -> Either String Command
parseSpawn [k] = case map toLower k of
  "rat"    -> Right (CmdSpawn Rat)
  "goblin" -> Right (CmdSpawn Goblin)
  "orc"    -> Right (CmdSpawn Orc)
  other    -> Left ("unknown monster kind: " ++ other)
parseSpawn _ = Left "usage: spawn <rat|goblin|orc>"

parseXP :: [String] -> Either String Command
parseXP [n] = case readMaybe n of
  Just i | i >= 0 -> Right (CmdXP i)
  _               -> Left "usage: xp N (N must be a non-negative integer)"
parseXP _ = Left "usage: xp N"
