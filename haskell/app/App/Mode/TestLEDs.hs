module App.Mode.TestLEDs (run) where

import Control.Concurrent
import Control.Monad

import Evdev.Uinput qualified as Uinput ()

import EurekaPROM.IO.ALSA   qualified as ALSA
import EurekaPROM.IO.Output qualified as Output

{-------------------------------------------------------------------------------
  Test the LEDs

  TODO: This should use the ALSA queue instead of using threadDelay.
-------------------------------------------------------------------------------}

run :: ALSA.Handle -> IO ()
run h = do
    forM_ [minBound .. maxBound] $ \led -> do
      print led
      Output.toggleLED h led True
      longDelay
      Output.toggleLED h led False
      shortDelay

    forM_ [0 .. 127] $ \x -> do
      let disp = Output.All $ Output.ValueInt x
      print disp
      Output.toggleDisplay h disp
      shortDelay
    Output.clearDisplay h

    forM_ [minBound .. maxBound] $ \x -> do
      let disp = Output.Leading $ Output.ValueLeading (Just x)
      print disp
      Output.toggleDisplay h disp
      longDelay
    Output.clearDisplay h

    let allHex = [minBound .. maxBound]
    forM_ (zip (Nothing : map Just allHex) allHex) $ \(mPrev, next) -> do
      let disp1 = Output.TensHex (Output.ValueHex mPrev)
      let disp2 = Output.OnesHex (Output.ValueHex (Just next))
      print (disp1, disp2)
      Output.toggleDisplay h disp1
      shortDelay
      Output.toggleDisplay h disp2
      longDelay
    Output.clearDisplay h

    let text = map Output.mkLetter "HEY  LIEVE  EVA  "
    forM_ (zip (Nothing : text) text) $ \(mPrev, mNext) -> do
      let disp1 = Output.TensLetter (Output.ValueLetter mPrev)
      let disp2 = Output.OnesLetter (Output.ValueLetter mNext)
      print (disp1, disp2)
      Output.toggleDisplay h disp1
      shortDelay
      Output.toggleDisplay h disp2
      threadDelay 1_000_000
  where
    longDelay, shortDelay :: IO ()
    longDelay  = threadDelay 500_000
    shortDelay = threadDelay 30_000
