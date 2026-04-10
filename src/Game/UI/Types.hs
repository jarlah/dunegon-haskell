-- | Shared UI-layer types that used to live inline in @app/Main.hs@.
--
--   Moving them into the library has two payoffs:
--
--   * The CLI flag parser ('parseArgs') becomes trivially testable
--     from HSpec — it was previously unreachable because
--     @app/Main.hs@ is not part of the library target.
--
--   * The UI sub-modules under "Game.UI.*" can close over the same
--     'RuntimeFlags' record without any of them having to import
--     from the executable.
--
--   We also re-export 'Name' from "Game.Render" here so that every
--   UI module has a single place to get both the Brick widget-name
--   type and the runtime flags from.
module Game.UI.Types
  ( RuntimeFlags (..)
  , parseArgs
  , Render.Name (..)
  ) where

import qualified Game.Render as Render

-- | Process-wide runtime capability flags that come from the
--   command line rather than from persistent state. Bundled into a
--   record so the event loop only has to close over one value, and
--   so new flags can be added without rewiring every handler.
--
--   Deliberately not in 'Game.Core.GameState' — these are
--   capabilities of /this/ process launch, not of the saved game,
--   and they must not round-trip through 'Data.Binary.Binary'. A
--   player running without @--wizard@ must not be able to invoke
--   cheats just because they loaded a save that was written by a
--   wizard session.
newtype RuntimeFlags = RuntimeFlags
  { rfWizardEnabled :: Bool
    -- ^ 'True' iff the game was launched with @--wizard@ (or one of
    --   its aliases). Gates the cheat / wizard slash commands and
    --   controls whether cheat-tainted saves are visible in the
    --   load menu.
  } deriving (Eq, Show)

-- | Parse the CLI flags we care about out of a raw 'getArgs'-style
--   argument list. Unknown flags are silently ignored — this is a
--   single-player game, not a user-facing CLI tool, so surfacing a
--   hard error on a typo would just be hostile.
parseArgs :: [String] -> RuntimeFlags
parseArgs args = RuntimeFlags
  { rfWizardEnabled =
      any (`elem` ["--wizard", "-w", "--cheats"]) args
  }
