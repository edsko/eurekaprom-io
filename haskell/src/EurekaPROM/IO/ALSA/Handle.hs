-- | ALSA state
--
-- Intended for qualified import.
--
-- > import EurekaPROM.IO.ALSA.Handle qualified as ALSA (Handle)
-- > import EurekaPROM.IO.ALSA.Handle qualified as Handle
module EurekaPROM.IO.ALSA.Handle (
    Handle(..)
  , init
  ) where

import Prelude hiding (init)

import "alsa-seq" Sound.ALSA.Sequencer         qualified as ALSA
import "alsa-seq" Sound.ALSA.Sequencer.Address qualified as Address
import "alsa-seq" Sound.ALSA.Sequencer.Client  qualified as Client
import "alsa-seq" Sound.ALSA.Sequencer.Port    qualified as Port
import "alsa-seq" Sound.ALSA.Sequencer.Queue   qualified as Queue

data Handle = Handle {
      alsa    :: ALSA.T ALSA.DuplexMode
    , client  :: Client.T
    , port    :: Port.T
    , address :: Address.T
    , queue   :: Queue.T
    }

init :: (Handle -> IO a) -> IO a
init k =
    ALSA.withDefault ALSA.Block                      $ \alsa  ->
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
        , Port.capRead
        , Port.capSubsRead
        , Port.capSyncRead
        , Port.capSyncWrite
        , Port.capDuplex

        ]

    portType :: Port.Type
    portType = Port.typeMidiGeneric
