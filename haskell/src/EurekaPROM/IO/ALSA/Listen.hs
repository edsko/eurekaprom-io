module EurekaPROM.IO.ALSA.Listen (
    listen
  ) where

import Control.Monad

import Sound.ALSA.Sequencer.Event     qualified as Event
import Sound.ALSA.Sequencer.Subscribe qualified as Subscribe

import EurekaPROM.IO.ALSA.Handle    qualified as ALSA (Handle)
import EurekaPROM.IO.ALSA.Handle    qualified as Handle
import EurekaPROM.IO.ALSA.MIDI      qualified as MIDI
import EurekaPROM.IO.ALSA.Discovery qualified as Discovery

-- | Process incoming events
--
-- This never returns.
listen :: ALSA.Handle -> Discovery.Port -> (MIDI.Message -> IO ()) -> IO a
listen h port k = do
    subscription <- Subscribe.malloc
    Subscribe.setSender subscription (Discovery.portAddress port)
    Subscribe.setDest subscription (Handle.address h)
    Subscribe.subscribePort (Handle.alsa h) subscription

    forever $ do
      event <- Event.input (Handle.alsa h)
      case MIDI.fromALSA event of
        Just msg -> k msg
        Nothing  -> return () -- TODO: warn?
