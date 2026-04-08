-- | IO shell for audio playback. Pure game logic emits 'GameEvent's;
--   this module is the only place that touches the audio device.
--
--   Everything here is graceful-degradation: if the audio device can't
--   be initialized, or a sample file is missing, the game keeps running
--   silently. A missing or broken audio backend must never crash the
--   game or block the render loop.
module Game.Audio
  ( AudioSystem
  , initAudio
  , shutdownAudio
  , playEvent
  ) where

import Control.Exception (try, SomeException)
import qualified Sound.ProteaAudio.SDL as PA

import Game.Types (GameEvent(..))

-- | Loaded sample handles plus a reference to the music loop, so we
--   can stop it on shutdown if we want to.
data AudioSystem = AudioSystem
  { asMiss    :: !PA.Sample
  , asHit     :: !PA.Sample
  , asCrit    :: !PA.Sample
  , asKill    :: !PA.Sample
  , asHurt    :: !PA.Sample
  , asDied    :: !PA.Sample
  , asLevelUp :: !PA.Sample
  }

-- | Initialise the audio backend, load every sample, and start the
--   background music loop. Returns 'Nothing' if *anything* goes wrong
--   — the caller should fall back to silent gameplay in that case.
initAudio :: IO (Maybe AudioSystem)
initAudio = do
  ok <- PA.initAudio 32 44100 1024
  if not ok
    then pure Nothing
    else do
      result <- (try $ do
        -- Music first: start looping as soon as the sample is loaded.
        music   <- PA.sampleFromFile "assets/music/theme.ogg" 0.35
        _       <- PA.soundLoop music 1.0 1.0 0.0 1.0
        -- SFX.
        miss    <- PA.sampleFromFile "assets/sfx/miss.ogg"    0.7
        hit     <- PA.sampleFromFile "assets/sfx/hit.ogg"     0.8
        crit    <- PA.sampleFromFile "assets/sfx/crit.ogg"    1.0
        kill    <- PA.sampleFromFile "assets/sfx/kill.ogg"    0.9
        hurt    <- PA.sampleFromFile "assets/sfx/hurt.ogg"    0.85
        died    <- PA.sampleFromFile "assets/sfx/died.ogg"    1.0
        levelUp <- PA.sampleFromFile "assets/sfx/levelup.ogg" 0.9
        pure AudioSystem
          { asMiss    = miss
          , asHit     = hit
          , asCrit    = crit
          , asKill    = kill
          , asHurt    = hurt
          , asDied    = died
          , asLevelUp = levelUp
          }) :: IO (Either SomeException AudioSystem)
      case result of
        Left _    -> PA.finishAudio >> pure Nothing
        Right sys -> pure (Just sys)

shutdownAudio :: AudioSystem -> IO ()
shutdownAudio _ = PA.finishAudio

-- | Fire the SFX for a single 'GameEvent'. Non-blocking. Exceptions
--   from the audio layer are swallowed so a glitch can't crash the
--   game.
playEvent :: AudioSystem -> GameEvent -> IO ()
playEvent as ev = do
  let sample = case ev of
        EvAttackMiss    -> asMiss    as
        EvAttackHit     -> asHit     as
        EvAttackCrit    -> asCrit    as
        EvMonsterKilled -> asKill    as
        EvPlayerHurt    -> asHurt    as
        EvPlayerDied    -> asDied    as
        EvLevelUp       -> asLevelUp as
        -- Quest turn-in reuses the level-up jingle for now — it's a
        -- positive progression sting, which is the same emotional
        -- beat. A dedicated asset can slot in later without touching
        -- the event pump.
        EvQuestTurnedIn -> asLevelUp as
  _ <- (try (PA.soundPlay sample 1.0 1.0 0.0 1.0))
         :: IO (Either SomeException PA.Sound)
  pure ()
