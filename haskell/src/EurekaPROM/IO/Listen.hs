module EurekaPROM.IO.Listen (
    listen
  ) where

import Control.Monad

import Sound.ALSA.Sequencer.Address   qualified as Address
import Sound.ALSA.Sequencer.Event     qualified as Event
import Sound.ALSA.Sequencer.Subscribe qualified as Subscribe

import EurekaPROM.IO.ALSA qualified as ALSA
import EurekaPROM.IO.MIDI qualified as MIDI

-- | Process incoming events
--
-- This never returns.
listen :: ALSA.Handle -> Address.T -> (MIDI.Message -> IO ()) -> IO a
listen h addr k = do
    subscription <- Subscribe.malloc
    Subscribe.setSender subscription addr
    Subscribe.setDest subscription (ALSA.address h)
    Subscribe.subscribePort (ALSA.alsa h) subscription

    forever $ do
      event <- Event.input (ALSA.alsa h)
      case MIDI.fromALSA event of
        Just msg -> k msg
        Nothing  -> return () -- TODO: warn?
