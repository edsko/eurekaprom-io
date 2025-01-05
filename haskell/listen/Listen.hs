module Listen (main) where

import Data.Map (Map)
import Data.Map qualified as Map
import System.IO

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
          -- NOTE: [KeyA .. KeyZ] does /not/ work (poor 'Enum' instance)
          Uinput.keys = [Codes.KeyE, Codes.KeyD, Codes.KeyS]
        }

processMsg :: Uinput.Device -> MIDI.Message -> IO ()
processMsg uinput msg = do
    case MIDI.msgBody msg of
      -- Note-on
      MIDI.MsgNote MIDI.Note{notePitch, noteVelocity}
        | noteVelocity == 100
        , Just events <- Map.lookup notePitch noteOnMap
        -> Uinput.writeBatch uinput events
      MIDI.MsgNote MIDI.Note{noteVelocity}
        | noteVelocity == 0
        -> return () -- Ignore note-off messages
      MIDI.MsgControl MIDI.Control{controlNumber}
        | controlNumber == 3
        -> return () -- Ignore CC3
      _otherwise
        -> hPutStrLn stderr $ concat [
                "Warning: unprocessed "
              , show msg
              ]
  where
    noteOnMap :: Map Int [Uinput.EventData]
    noteOnMap = Map.fromList [
          (60, pressAndRelease Codes.KeyE)
        , (61, pressAndRelease Codes.KeyD)
        , (62, pressAndRelease Codes.KeyS)
        ]

    pressAndRelease :: Codes.Key -> [Uinput.EventData]
    pressAndRelease key = [
          Uinput.KeyEvent key Uinput.Pressed
        , Uinput.KeyEvent key Uinput.Released
        ]
