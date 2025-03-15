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
main = ALSA.init $ \h -> do
    cmdline <- getCmdline
    case cmdMode cmdline of
      ModeListPorts ->
        ALSA.listPorts h
      ModeDump portSpec -> do
        ALSA.resolve h portSpec
        Mode.Dump.run h
      ModeSimEvents portSpec -> do
        ALSA.resolve h portSpec
        Mode.SimEvents.run h
      ModeTestLEDs portSpec -> do
        ALSA.resolve h portSpec
        Mode.TestLEDs.run h
      ModeGenMealy ->
        Mode.GenMealy.run
