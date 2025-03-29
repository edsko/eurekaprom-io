-- | ALSA state
--
-- Intended for qualified import.
--
-- > import Control.ALSA.Handle qualified as ALSA (Handle)
-- > import Control.ALSA.Handle qualified as Handle
module Control.ALSA.Handle (
    Handle(..)
  , open
  , close
  , with
  ) where

import Prelude hiding (init)
import Control.Exception

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

open :: IO Handle
open = do
    -- Open handle
    alsa <- ALSA.openDefault ALSA.Block

    -- Configure client
    Client.setName alsa "eurekaprom-io"
    client <- Client.getId alsa

    -- Create default port
    port <- Port.createSimple alsa "default" portCaps portType
    let address = Address.Cons {
            client = client
          , port   = port
          }

    -- Create default queue
    queue <- Queue.allocNamed alsa "default"

    -- Done
    return Handle{alsa, client, port, address, queue}
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


close :: Handle -> IO ()
close Handle{alsa, port, queue} = do
    -- Release in opposite order
    Queue.free alsa queue
    Port.deleteSimple alsa port
    ALSA.close alsa

with :: (Handle -> IO a) -> IO a
with = bracket open close
