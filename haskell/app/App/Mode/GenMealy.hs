{-# LANGUAGE OverloadedStrings #-}

module App.Mode.GenMealy (Cmd(..), run) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as Map
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
      Exec h -> do
        Mealy.exec machine Mealy.ExecEnv{
            produceInput  = waitInputUsing Input.pedalEventFromMIDI h
          , processOutput = print
          , unrecognized  = warnUnrecognized
          , initialState  = Simultaneous.initDeviceState
          , finalState    = const False
          }
      Yaml fp ->
        Yaml.encodeFile fp . YamlOutput $ machine
      Json fp ->
        Aeson.encodeFile fp . JsonOutput $ Mealy.encodeState machine
  where
    machine :: Simultaneous.MealyMachine
    machine = simultaneous

    warnUnrecognized :: Simultaneous.DeviceState -> Input.PedalEvent -> IO ()
    warnUnrecognized s i =
        putStrLn $ "No transition for " ++ show i ++ " in state " ++ show s


{-
{-------------------------------------------------------------------------------
  Configure YAML output
-------------------------------------------------------------------------------}
-}

newtype YamlOutput a = YamlOutput a
  deriving stock (Eq, Ord)

yaml :: ToJSON (YamlOutput a) => a -> Yaml.Value
yaml = toJSON . YamlOutput

instance ToJSON (YamlOutput a) => ToJSON (YamlOutput [a]) where
  toJSON (YamlOutput xs) = toJSON (map yaml xs)

instance ToJSON (YamlOutput Simultaneous.MealyMachine) where
  toJSON (YamlOutput machine) = yaml (Mealy.toTransitions machine)

instance ( ToJSON (YamlOutput s)
         , ToJSON (YamlOutput i)
         , ToJSON (YamlOutput o)
         ) => ToJSON (YamlOutput (Mealy.Transition s i o)) where
  toJSON (YamlOutput Mealy.Transition{from = (s, i), to = (s', o)}) = object [
        "from" .= object [
            "state" .= yaml s
          , "input" .= yaml i
          ]
      , "to" .= object [
            "state"  .= yaml s'
          , "output" .= yaml o
          ]
      ]

instance ToJSON (YamlOutput Simultaneous.DeviceState) where
  toJSON (YamlOutput state) =
      case state of
        Simultaneous.StateNoPedals ->
          yaml ([] :: [Input.Pedal])
        Simultaneous.StateOnePedal pedal ->
          yaml [pedal]
        Simultaneous.StateTwoPedals pedal1 pedal2 ->
          yaml [pedal1, pedal2]

instance ToJSON (YamlOutput Input.PedalEvent) where
  toJSON (YamlOutput inputEvent) =
      case inputEvent of
        Input.PedalEvent pedal Input.Press -> object [
            "press" .= yaml pedal
          ]
        Input.PedalEvent pedal Input.Release -> object [
            "release" .= yaml pedal
          ]

instance ToJSON (YamlOutput Input.Pedal) where
  toJSON (YamlOutput pedal) = toJSON $ show pedal

{-------------------------------------------------------------------------------
  Configure JSON output

  The JSON output just records the controller number and value.
-------------------------------------------------------------------------------}

newtype JsonOutput a = JsonOutput a
  deriving stock (Eq, Ord)

json :: ToJSON (JsonOutput a) => a -> Yaml.Value
json = toJSON . JsonOutput

instance ToJSON (JsonOutput a) => ToJSON (JsonOutput [a]) where
  toJSON (JsonOutput xs) = toJSON (map json xs)

instance ToJSON (JsonOutput (Mealy Int Input.PedalEvent [Input.PedalEvent])) where
  toJSON (JsonOutput machine) = object [
        Key.fromString (show s) .= object [
            Key.fromString (show ccNum) .= object [
                Key.fromString (show ccVal) .= object [
                    "state"  .= toState
                  , "output" .= json output
                  ]
              | (ccVal, (toState, output)) <- Map.toList fromCcNum
              ]
          | (ccNum, fromCcNum) <- Map.toList $ byCC fromS
          ]
      | (s, fromS) <- Map.toList $ Mealy.transitions machine
      ]

instance ToJSON (JsonOutput Input.PedalEvent) where
  toJSON (JsonOutput inputEvent) = toJSON $ pedalEventCcNumVal inputEvent

pedalEventCcNumVal :: Input.PedalEvent -> (Int, Int)
pedalEventCcNumVal event =
    case MIDI.messageBody $ Input.pedalEventToMIDI event of
      MIDI.MsgControl control -> (
          MIDI.controlNumber control
        , MIDI.controlValue  control
        )
      _otherwise ->
        error "unexpected MIDI message"

byCC :: forall a. Map Input.PedalEvent a -> Map Int (Map Int a)
byCC =
      Map.unionsWith Map.union
    . map singleton
    . map (first pedalEventCcNumVal)
    . Map.toList
  where
    singleton :: ((Int, Int), a) -> Map Int (Map Int a)
    singleton ((num, val), a) = Map.singleton num $ Map.singleton val a
