-- | Utilities for working with ALSA
--
-- Intended for qualified import.
module EurekaPROM.IO.Alsa (
    Handle(..)
  , init
  ) where

import Prelude hiding (init)

import Sound.ALSA.Sequencer qualified as Alsa

data Handle = Handle {
      alsa :: Alsa.T Alsa.DuplexMode
    }

init :: (Handle -> IO a) -> IO a
init k =
    Alsa.withDefault @Alsa.DuplexMode Alsa.Block $ \alsa ->
      k Handle{alsa}
