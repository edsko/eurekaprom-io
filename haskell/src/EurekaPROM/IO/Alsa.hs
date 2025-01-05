-- | Utilities for working with ALSA
--
-- Intended for qualified import.
module EurekaPROM.IO.Alsa (
    Handle(..)
  , init
  ) where

import Prelude hiding (init)

import Sound.ALSA.Sequencer         qualified as Alsa
import Sound.ALSA.Sequencer.Address qualified as Address
import Sound.ALSA.Sequencer.Client  qualified as Client
import Sound.ALSA.Sequencer.Port    qualified as Port
import Sound.ALSA.Sequencer.Queue   qualified as Queue

data Handle = Handle {
      alsa    :: Alsa.T Alsa.DuplexMode
    , client  :: Client.T
    , port    :: Port.T
    , address :: Address.T
    , queue   :: Queue.T
    }

init :: (Handle -> IO a) -> IO a
init k =
    Alsa.withDefault @Alsa.DuplexMode Alsa.Block     $ \alsa  ->
    Port.withSimple alsa "default" portCaps portType $ \port  ->
    Queue.withNamed alsa "default"                   $ \queue ->
    do
      Client.setName alsa "eurekaprom-io"
      client <- Client.getId alsa
      let address = Address.Cons {
              client = client
            , port   = port
            }
      k Handle{alsa, client, port, address, queue}
  where
    portCaps :: Port.Cap
    portCaps = Port.caps [
          Port.capWrite
        , Port.capSubsWrite
        ]

    portType :: Port.Type
    portType = Port.typeMidiGeneric
