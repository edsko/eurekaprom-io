-- | Interface with the EurekaPROM in IO mode
--
-- The goal is that applications working with the EurekaPROM only need to import
-- this one module, with no need to import from @Sound.ALSA.Sequencer.*@.
module EurekaPROM.IO (
    -- * ALSA utilities
    ALSA.Handle -- opaque
  , ALSA.init
    -- * Discovery
  , Discovery.Client(..)
  , Discovery.Port(..)
  , Discovery.clientPortName
  , Discovery.getAllPorts
  , Discovery.FindPortResult(..)
  , Discovery.findPort
    -- * Communication
  , Listen.listen
  ) where

import EurekaPROM.IO.ALSA      qualified as ALSA
import EurekaPROM.IO.Discovery qualified as Discovery
import EurekaPROM.IO.Listen    qualified as Listen
