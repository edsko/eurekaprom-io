module App.Mode.Dump (run) where

import Control.Monad

import Control.ALSA.Handle qualified as ALSA (Handle)

import EurekaPROM.IO.ALSA

{-------------------------------------------------------------------------------
  Dump
-------------------------------------------------------------------------------}

run :: ALSA.Handle -> IO ()
run h = forever $ print =<< waitInput h
