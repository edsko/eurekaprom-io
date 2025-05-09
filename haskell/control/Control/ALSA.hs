-- | High-level utilities for working with ALSA MIDI devices
--
-- Intended for qualified import.
--
-- > import Control.ALSA qualified as ALSA
module Control.ALSA (
    -- Resolution
    PortSpec(..)
  , getPortNames
  , listPorts
  , resolve
  ) where

import "alsa-seq" Sound.ALSA.Sequencer.Subscribe qualified as Subscribe

import "midi-alsa" Sound.MIDI.ALSA.Query ()
import "midi-alsa" Sound.MIDI.ALSA.Construct ()

import Control.ALSA.Discovery (PortName)
import Control.ALSA.Discovery qualified as Discovery
import Control.ALSA.Handle qualified as ALSA (Handle)
import Control.ALSA.Handle qualified as Handle

{-------------------------------------------------------------------------------
  Resolution
-------------------------------------------------------------------------------}

data PortSpec =
    -- | We use a single port for input and output
    PortDuplex PortName

    -- | Separate ports for input and output
  | PortDual {
        inputPort  :: PortName
      , outputPort :: PortName
      }

    -- | Configure for input only
  | PortInputOnly PortName

    -- | Configure for output only
  | PortOutputOnly PortName
  deriving stock (Show)

getPortNames :: ALSA.Handle -> IO [String]
getPortNames h = map Discovery.portQualifiedName <$> Discovery.getAllPorts h

listPorts :: ALSA.Handle -> IO ()
listPorts h = do
    portNames <- getPortNames h
    mapM_ putStrLn portNames

resolve :: ALSA.Handle -> PortSpec -> IO ()
resolve _ (PortDuplex _) =
    error "TODO: PortDuplex"
resolve h (PortInputOnly inp) = do
    inp' <- Discovery.findPort h inp
    sub  <- Subscribe.malloc
    Subscribe.setSender sub (Discovery.portAddress inp')
    Subscribe.setDest sub (Handle.address h)
    Subscribe.subscribePort (Handle.alsa h) sub
resolve h (PortOutputOnly out) = do
    out' <- Discovery.findPort h out
    sub  <- Subscribe.malloc
    Subscribe.setSender sub (Handle.address h)
    Subscribe.setDest sub (Discovery.portAddress out')
    Subscribe.subscribePort (Handle.alsa h) sub
resolve h (PortDual inp out) = do
    resolve h (PortInputOnly  inp)
    resolve h (PortOutputOnly out)
