-- | High-level utilities for working with ALSA MIDI devices
--
-- Intended for qualified import.
--
-- > import Control.ALSA qualified as ALSA
module Control.ALSA (
    -- Resolution
    PortSpec(..)
  , listPorts
  , resolve
  ) where

import Control.Monad

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
    SinglePort PortName

    -- | Separate ports for input and output
  | DualPort {
        inputPort  :: PortName
      , outputPort :: PortName
      }
  deriving stock (Show)

listPorts :: ALSA.Handle -> IO ()
listPorts h = do
    ports <- Discovery.getAllPorts h
    forM_ ports $ \port ->
      putStrLn $ Discovery.portQualifiedName port

resolve :: ALSA.Handle -> PortSpec -> IO ()
resolve _ (SinglePort _) =
    error "TODO: SinglePort"
resolve h (DualPort inp out) = do
    inp' <- Discovery.findPort h inp
    out' <- Discovery.findPort h out

    do sub <- Subscribe.malloc
       Subscribe.setSender sub (Discovery.portAddress inp')
       Subscribe.setDest sub (Handle.address h)
       Subscribe.subscribePort (Handle.alsa h) sub

    do sub <- Subscribe.malloc
       Subscribe.setSender sub (Handle.address h)
       Subscribe.setDest sub (Discovery.portAddress out')
       Subscribe.subscribePort (Handle.alsa h) sub
