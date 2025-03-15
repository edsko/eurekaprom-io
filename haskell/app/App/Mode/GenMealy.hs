module App.Mode.GenMealy (Cmd(..), run) where

import Data.Aeson (ToJSON(..))
import Data.Aeson qualified as Aeson
import Data.Yaml qualified as Yaml

import Control.ALSA.Handle qualified as ALSA (Handle)
import EurekaPROM.IO.Input qualified as Input
import EurekaPROM.IO.Simultaneous (simultaneous)
import EurekaPROM.IO.Simultaneous qualified as Simultaneous

import Data.Mealy (Mealy)
import Data.Mealy qualified as Mealy
import Data.MIDI qualified as MIDI

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
            produceInput  = Input.wait h
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
    machine :: Mealy Simultaneous.DeviceState Input.Event [Input.Event]
    machine = simultaneous

    warnUnrecognized :: Simultaneous.DeviceState -> Input.Event -> IO ()
    warnUnrecognized s i =
        putStrLn $ "No transition for " ++ show i ++ " in state " ++ show s

{-------------------------------------------------------------------------------
  Simple example: configure pedal 1 as a sustain pedal
-------------------------------------------------------------------------------}

data SustainState = SustainReleased | SustainPressed
  deriving stock (Show, Eq, Ord)

sustain :: Mealy SustainState Input.Event MIDI.Message
sustain = Mealy.fromTransitions [
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

newtype YamlOutput a = YamlOutput a
  deriving stock (Eq, Ord)

instance ToJSON (YamlOutput Simultaneous.DeviceState) where
  toJSON (YamlOutput state) =
      case state of
        Simultaneous.NoPedalsPressed ->
          toJSON "no pedals pressed"
        Simultaneous.OnePedalPressed pedal1 ->
          toJSON $ "pressed: " ++ show pedal1
        Simultaneous.TwoPedalsPressed pedal1 pedal2 ->
          toJSON $ "pressed: " ++ show pedal1 ++ " and " ++ show pedal2

instance ToJSON (YamlOutput Input.Event) where
  toJSON (YamlOutput inputEvent) =
      case inputEvent of
        Input.EventPedal pedal Input.Press ->
          toJSON $ "press " ++ show pedal
        Input.EventPedal pedal Input.Release ->
          toJSON $ "release " ++ show pedal
        Input.EventExpr{} ->
          error "TODO"

instance ToJSON (YamlOutput [Input.Event]) where
  toJSON (YamlOutput outputEvents) = toJSON $
      map (toJSON . YamlOutput) outputEvents

{-------------------------------------------------------------------------------
  Configure JSON output
-------------------------------------------------------------------------------}

newtype JsonOutput a = JsonOutput a
  deriving stock (Eq, Ord)

instance ToJSON (JsonOutput Mealy.StateNum) where
  toJSON (JsonOutput state) = toJSON state

instance ToJSON (JsonOutput Input.Event) where
  toJSON (JsonOutput event) = toJSON (show event) -- TODO

instance ToJSON (JsonOutput [Input.Event]) where
  toJSON (JsonOutput outputEvents) = toJSON $
      map (toJSON . JsonOutput) outputEvents
