module EurekaPROM.IO.Listen (
    listen
  ) where

import Control.Monad

import Sound.ALSA.Sequencer.Address   qualified as Address
import Sound.ALSA.Sequencer.Event     qualified as Event
import Sound.ALSA.Sequencer.Subscribe qualified as Subscribe

import EurekaPROM.IO.Alsa qualified as Alsa

-- | Process incoming events
--
-- This never returns.
listen :: Alsa.Handle -> Address.T -> (Event.T -> IO ()) -> IO a
listen h addr k = do
    subscription <- Subscribe.malloc
    Subscribe.setSender subscription addr
    Subscribe.setDest subscription (Alsa.address h)
    Subscribe.subscribePort (Alsa.alsa h) subscription

    forever $ do
      event <- Event.input (Alsa.alsa h)
      k event
