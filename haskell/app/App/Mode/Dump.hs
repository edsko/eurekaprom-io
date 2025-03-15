module App.Mode.Dump (run) where

import Control.Monad

import Evdev.Uinput qualified as Uinput ()

import EurekaPROM.IO.ALSA   qualified as ALSA
import EurekaPROM.IO.Input  qualified as Input

{-------------------------------------------------------------------------------
  Dump
-------------------------------------------------------------------------------}

run :: ALSA.Handle -> IO ()
run h = forever $ print =<< Input.wait h
