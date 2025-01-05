module Listen (main) where

import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map
import System.IO

import Evdev.Codes  qualified as Codes
import Evdev.Uinput qualified as Uinput ()

import EurekaPROM.IO.ALSA qualified as ALSA
import EurekaPROM.IO.App  qualified as App
import EurekaPROM.IO.MIDI qualified as MIDI

import Listen.Adaptor (Adaptor)
import Listen.Adaptor qualified as Adaptor
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
listen h port = Adaptor.run $ ALSA.listen h port processMsg

processMsg :: MIDI.Message -> Adaptor ()
processMsg msg = do
    case MIDI.msgBody msg of
      -- Note-on
      MIDI.MsgNote MIDI.Note{notePitch, noteVelocity}
        | noteVelocity == 100
        , Just events <- Map.lookup notePitch noteOnMap
        -> do Adaptor.writeInputEvents events

      -- Note-off (currently ignored)
      MIDI.MsgNote MIDI.Note{noteVelocity}
        | noteVelocity == 0
        -> do return () -- Ignore note-off messages

      -- Ignore CC3
      MIDI.MsgControl MIDI.Control{controlNumber}
        | controlNumber == 3
        -> do return ()

      -- First expression pedal
      MIDI.MsgControl ctrl@MIDI.Control{controlNumber}
        | controlNumber == 84
        -> do mDelta <- Adaptor.deltaCC ctrl
              case mDelta of
                Nothing ->
                  -- Ignore first message
                  return ()
                Just d | d > 0 ->
                  Adaptor.writeInputEvents [Adaptor.Rel (5, 0)]
                Just _ | otherwise ->
                  Adaptor.writeInputEvents [Adaptor.Rel (-5, 0)]

      -- Unrecognized messages
      _otherwise
        -> do liftIO $ hPutStrLn stderr $ "Warning: unprocessed " ++ show msg
  where
    noteOnMap :: Map Int [Adaptor.InputEvent]
    noteOnMap = Map.fromList [
          (60, [Adaptor.Key Codes.KeyE])
        , (61, [Adaptor.Key Codes.KeyD])
        , (62, [Adaptor.Key Codes.KeyS])
        ]
