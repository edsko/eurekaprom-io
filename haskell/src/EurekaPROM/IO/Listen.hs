module EurekaPROM.IO.Listen (
    listen
  ) where

import Sound.ALSA.Sequencer.Address   qualified as Address
import Sound.ALSA.Sequencer.Subscribe qualified as Subscribe

import EurekaPROM.IO.Alsa qualified as Alsa

listen :: Alsa.Handle -> Address.T -> IO ()
listen Alsa.Handle{alsa} port = do
    Subscribe.subscribePort alsa undefined
