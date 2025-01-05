module Listen (main) where

import Evdev.Codes  qualified as Codes
import Evdev.Uinput qualified as Uinput

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
listen h port = do
    uinput <- Uinput.newDevice "eurekaprom-io-listen" deviceOpts
    ALSA.listen h port $ processMsg uinput
  where
    deviceOpts :: Uinput.DeviceOpts
    deviceOpts = Uinput.defaultDeviceOpts{
          Uinput.keys = [Codes.KeyA .. Codes.KeyZ]
        }

processMsg :: Uinput.Device -> MIDI.Message -> IO ()
processMsg uinput msg = do
    print msg
    case MIDI.msgBody msg of
      MIDI.MsgNote MIDI.Note{notePitch, noteVelocity} | notePitch == 60, noteVelocity == 100 ->
        Uinput.writeBatch uinput [
            Uinput.KeyEvent Codes.KeyA Uinput.Pressed
          , Uinput.KeyEvent Codes.KeyA Uinput.Released
          ]
      _otherwise ->
        return ()
