{-# LANGUAGE OverloadedStrings #-}

module App.Mode.GenMealy (Cmd(..), run) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Yaml qualified as Yaml

import Control.ALSA.Handle qualified as ALSA (Handle)
import Data.Mealy (Mealy)
import Data.Mealy qualified as Mealy
import Data.MIDI qualified as MIDI

import EurekaPROM.IO.ALSA
import EurekaPROM.IO.Input qualified as Input
import EurekaPROM.IO.Simultaneous (simultaneous)
import EurekaPROM.IO.Simultaneous qualified as Simultaneous

{-------------------------------------------------------------------------------
  Generate Mealy machine
-------------------------------------------------------------------------------}

data Cmd a =
    Exec a
  | Yaml FilePath
  | Json FilePath
  deriving stock (Show)

run :: Cmd ALSA.Handle -> IO ()
run mode =
    case mode of
      Exec h ->
        Mealy.exec machine Mealy.ExecEnv{
            produceInput  = waitInput h
          , processOutput = print
          , unrecognized  = warnUnrecognized
          , initialState  = Simultaneous.initDeviceState
          , finalState    = const False
          }
      Yaml fp ->
        Yaml.encodeFile fp $
          Mealy.wrap YamlOutput machine
      Json fp ->
        Aeson.encodeFile fp $
          Mealy.wrap JsonOutput $ Mealy.encodeState machine
  where
    machine :: Simultaneous.MealyMachine
    machine = simultaneous

    warnUnrecognized :: Simultaneous.DeviceState -> Input.Event -> IO ()
    warnUnrecognized s i =
        putStrLn $ "No transition for " ++ show i ++ " in state " ++ show s

{-------------------------------------------------------------------------------
  Simple example: configure pedal 1 as a sustain pedal
-------------------------------------------------------------------------------}

data SustainState = SustainReleased | SustainPressed
  deriving stock (Show, Eq, Ord)

_sustain :: Mealy SustainState Input.Event MIDI.Message
_sustain = Mealy.fromTransitions [
      Mealy.Transition {
          from   = SustainReleased
        , input  = Input.EventPedal Input.Pedal1 Input.Press
        , to     = SustainPressed
        , output = MIDI.Message {
              messageChannel = 0
            , messageBody    = MIDI.MsgControl MIDI.Control {
                  controlNumber = 64
                , controlValue  = 127
                }
            }
        }
    , Mealy.Transition {
          from   = SustainPressed
        , input  = Input.EventPedal Input.Pedal1 Input.Release
        , to     = SustainReleased
        , output = MIDI.Message {
              messageChannel = 0
            , messageBody    = MIDI.MsgControl MIDI.Control {
                  controlNumber = 64
                , controlValue  = 0
                }
            }
        }
    ]


{-------------------------------------------------------------------------------
  Configure YAML output
-------------------------------------------------------------------------------}

toYaml :: ToJSON (YamlOutput a) => a -> Yaml.Value
toYaml = toJSON . YamlOutput

newtype YamlOutput a = YamlOutput a
  deriving stock (Eq, Ord)

instance ToJSON (YamlOutput a) => ToJSON (YamlOutput [a]) where
  toJSON (YamlOutput xs) = toJSON (map toYaml xs)

instance ToJSON (YamlOutput Simultaneous.DeviceState) where
  toJSON (YamlOutput state) =
      case state of
        Simultaneous.StateNoPedals -> object [
            "pedals" .= toYaml ([] :: [Input.Pedal])
          ]
        Simultaneous.StateOnePedal onePedal ->
          toYaml onePedal
        Simultaneous.StateTwoPedals twoPedals ->
          toYaml twoPedals

instance ToJSON (YamlOutput Simultaneous.OnePedalPressed) where
  toJSON (YamlOutput onePedal) = object [
        "pedals"    .= toYaml [pedal]
      , "notified1" .= notified
      ]
    where
      Simultaneous.OnePedalPressed pedal notified = onePedal

instance ToJSON (YamlOutput Simultaneous.TwoPedalsPressed) where
  toJSON (YamlOutput twoPedals) = object [
        "pedals"    .= toYaml [pedal1, pedal2]
      , "notified1" .= notified1
      ]
    where
      Simultaneous.TwoPedalsPressed pedal1 pedal2 notified1 = twoPedals

instance ToJSON (YamlOutput Input.Event) where
  toJSON (YamlOutput inputEvent) =
      case inputEvent of
        Input.EventPedal pedal Input.Press -> object [
            "press" .= toYaml pedal
          ]
        Input.EventPedal pedal Input.Release -> object [
            "release" .= toYaml pedal
          ]
        Input.EventExpr{} ->
          error "unimplemented"

instance ToJSON (YamlOutput Input.Pedal) where
  toJSON (YamlOutput pedal) = toJSON $ show pedal

{-------------------------------------------------------------------------------
  Configure JSON output

  The JSON output just records the controller number and value.
-------------------------------------------------------------------------------}

newtype JsonOutput a = JsonOutput a
  deriving stock (Eq, Ord)

instance ToJSON (JsonOutput Mealy.StateNum) where
  toJSON (JsonOutput state) = toJSON state

instance ToJSON (JsonOutput Input.Event) where
  toJSON (JsonOutput inputEvent) =
      case MIDI.messageBody $ Input.toMIDI inputEvent of
        MIDI.MsgControl control ->
          toJSON [
              toJSON $ MIDI.controlNumber control
            , toJSON $ MIDI.controlValue  control
            ]
        _otherwise ->
          error "unexpected MIDI message"

instance ToJSON (JsonOutput [Input.Event]) where
  toJSON (JsonOutput outputEvents) = toJSON $
      map (toJSON . JsonOutput) outputEvents
