module Listen (main) where

import EurekaPROM.IO.ALSA qualified as ALSA
import EurekaPROM.IO.App  qualified as App
import EurekaPROM.IO.MIDI qualified as MIDI

import Listen.Cmdline

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

main :: IO ()
main = ALSA.init $ \h -> do
    cmdline <- getCmdline
    case cmdMode cmdline of
      ModeListPorts ->
        App.listPorts h
      ModeListen portName ->
        App.findPort h portName $ listen h

{-------------------------------------------------------------------------------
  Listen for and process MIDI messages
-------------------------------------------------------------------------------}

listen :: ALSA.Handle -> ALSA.Port -> IO ()
listen h port = ALSA.listen h port processMsg

processMsg :: MIDI.Message -> IO ()
processMsg = print
