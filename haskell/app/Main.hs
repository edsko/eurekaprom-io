module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map

import Evdev.Codes  qualified as Codes
import Evdev.Uinput qualified as Uinput ()

import EurekaPROM.IO.ALSA   qualified as ALSA
import EurekaPROM.IO.Input  qualified as Input
import EurekaPROM.IO.Output qualified as Output

import App.Adaptor qualified as Adaptor
import App.Cmdline

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
        dump h
      ModeSimEvents portSpec -> do
        ALSA.resolve h portSpec
        simEvents h
      ModeTestLEDs portSpec -> do
        ALSA.resolve h portSpec
        testLEDs h

{-------------------------------------------------------------------------------
  Dump
-------------------------------------------------------------------------------}

dump :: ALSA.Handle -> IO ()
dump h = forever $ print =<< Input.wait h

{-------------------------------------------------------------------------------
  Simulate events

  This is just a proof of concept right now. We should make the events
  configurable.
-------------------------------------------------------------------------------}

simEvents :: ALSA.Handle -> IO ()
simEvents h = Adaptor.run $ forever $ do
    ev <- liftIO $ Input.wait h
    case ev of
      Input.EventPedal pedal Input.Press ->
        Adaptor.writeInputEvents $ Map.findWithDefault [] pedal noteOnMap
      Input.EventPedal _pedal Input.Release ->
        -- Currently ignored
        return ()
      Input.EventExpr Input.ExprA value -> do
        mDelta <- Adaptor.deltaCC 102 value
        case mDelta of
          Nothing ->
            -- Ignore first message
            return ()
          Just d | d > 0 ->
            Adaptor.writeInputEvents [Adaptor.Rel (5, 0)]
          Just _ | otherwise ->
            Adaptor.writeInputEvents [Adaptor.Rel (-5, 0)]
      Input.EventExpr Input.ExprB _value ->
        -- Currently ignored
        return ()
  where
    noteOnMap :: Map Input.Pedal [Adaptor.InputEvent]
    noteOnMap = Map.fromList [
          (Input.Pedal1, [Adaptor.Key Codes.KeyE])
        , (Input.Pedal2, [Adaptor.Key Codes.KeyD])
        , (Input.Pedal3, [Adaptor.Key Codes.KeyS])
        ]

{-------------------------------------------------------------------------------
  Test the LEDs

  TODO: This should use the ALSA queue instead of using threadDelay.
-------------------------------------------------------------------------------}

testLEDs :: ALSA.Handle -> IO ()
testLEDs h = do
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
