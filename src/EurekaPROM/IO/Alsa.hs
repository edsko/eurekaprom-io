-- | Utilities for working with ALSA
module EurekaPROM.IO.Alsa (
    init
  ) where

import Prelude hiding (init)

import Sound.ALSA.Sequencer qualified as Alsa

init :: (Alsa.T Alsa.DuplexMode -> IO a) -> IO a
init k = Alsa.withDefault @Alsa.DuplexMode Alsa.Block k