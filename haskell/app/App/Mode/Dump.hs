module App.Mode.Dump (run) where

import Control.Monad

import Evdev.Uinput qualified as Uinput ()

import Control.ALSA.Handle qualified as ALSA (Handle)
import EurekaPROM.IO.Input qualified as Input

{-------------------------------------------------------------------------------
  Dump
-------------------------------------------------------------------------------}

run :: ALSA.Handle -> IO ()
run h = forever $ print =<< Input.wait h
