-- | Pure parser for the slash-command prompt.
--
--   The prompt layer (render + input) lives in 'Main' / 'Render';
--   everything reachable through the prompt is defined here as a
--   plain ADT and a pure @parseCommand@ function so it can be
--   unit-tested without touching Brick.
--
--   This starts small on purpose: only @/reveal@ is wired up, as a
--   proof-of-concept wizard command. Expanding the command set is a
--   matter of adding a constructor + a parser case — no input or
--   rendering changes needed.
module Game.Logic.Command
  ( Command(..)
  , parseCommand
  ) where

import Data.Char (toLower)

-- | A parsed prompt command. Only the debug "reveal the whole map"
--   command is implemented right now; see the M9 plan entry for
--   the intended full catalogue.
data Command
  = CmdReveal
  deriving (Eq, Show)

-- | Parse a raw prompt buffer into a 'Command'. Leading whitespace
--   and a single leading @/@ are tolerated so the user can type
--   either @reveal@ or @/reveal@.
parseCommand :: String -> Either String Command
parseCommand raw =
  case words (stripLeadSlash (dropWhile (== ' ') raw)) of
    []        -> Left "empty command"
    (w : _) -> case map toLower w of
      "reveal" -> Right CmdReveal
      other    -> Left ("unknown command: " ++ other)
  where
    stripLeadSlash ('/' : rest) = rest
    stripLeadSlash s            = s
