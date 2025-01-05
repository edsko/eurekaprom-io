-- | Utilities for working with ALSA MIDI devices
--
-- The goal is that applications working with the EurekaPROM only need to import
-- this one module, with no need to import from @Sound.ALSA.Sequencer.*@.
module EurekaPROM.IO.ALSA (
    -- * Initialization
    ALSA.Handle -- opaque
  , Handle.init
    -- * Discovery
  , Discovery.Client(..)
  , Discovery.Port(..)
  , Discovery.portQualifiedName
  , Discovery.getAllPorts
  , Discovery.FindPortResult(..)
  , Discovery.findPort
    -- * Communication
  , Listen.listen
  ) where

import EurekaPROM.IO.ALSA.Handle    qualified as ALSA (Handle)
import EurekaPROM.IO.ALSA.Handle    qualified as Handle
import EurekaPROM.IO.ALSA.Discovery qualified as Discovery
import EurekaPROM.IO.ALSA.Listen    qualified as Listen
