module App.Mode.SimEvents (run) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map

import Evdev.Codes qualified as Codes
import Evdev.Uinput qualified as Uinput ()

import Control.ALSA.Handle qualified as ALSA (Handle)

import EurekaPROM.IO.Input qualified as Input
import EurekaPROM.IO.ALSA

import App.Adaptor qualified as Adaptor

{-------------------------------------------------------------------------------
  Simulate events

  This is just a proof of concept right now. We should make the events
  configurable.

  TODO: Ideally this would make use of the Mealy machine we generate.
-------------------------------------------------------------------------------}

run :: ALSA.Handle -> IO ()
run h = Adaptor.run $ forever $ do
    ev <- liftIO $ waitInput h
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
