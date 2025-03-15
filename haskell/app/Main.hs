module Main (main) where

import Evdev.Uinput qualified as Uinput ()

import EurekaPROM.IO.ALSA   qualified as ALSA

import App.Cmdline

import App.Mode.Dump      qualified as Mode.Dump
import App.Mode.GenMealy  qualified as Mode.GenMealy
import App.Mode.SimEvents qualified as Mode.SimEvents
import App.Mode.TestLEDs  qualified as Mode.TestLEDs

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline <- getCmdline
    case cmdMode cmdline of
      ModeListPorts ->
        ALSA.init $ \h -> do
          ALSA.listPorts h
      ModeDump portSpec -> do
        ALSA.init $ \h -> do
          ALSA.resolve h portSpec
          Mode.Dump.run h
      ModeSimEvents portSpec -> do
        ALSA.init $ \h -> do
          ALSA.resolve h portSpec
          Mode.SimEvents.run h
      ModeTestLEDs portSpec -> do
        ALSA.init $ \h -> do
          ALSA.resolve h portSpec
          Mode.TestLEDs.run h
      ModeGenMealy cmd ->
        initGenMealy cmd Mode.GenMealy.run

-- Avoid initializing ALSA when we just export a Mealy machine
initGenMealy ::
     Mode.GenMealy.Cmd ALSA.PortSpec
  -> (Mode.GenMealy.Cmd ALSA.Handle -> IO r)
  -> IO r
initGenMealy (Mode.GenMealy.Exec portSpec) k =
    ALSA.init $ \h -> do
      ALSA.resolve h portSpec
      k $ Mode.GenMealy.Exec h
initGenMealy (Mode.GenMealy.Yaml fp) k =
    k $ Mode.GenMealy.Yaml fp
initGenMealy (Mode.GenMealy.Json fp) k =
    k $ Mode.GenMealy.Json fp
