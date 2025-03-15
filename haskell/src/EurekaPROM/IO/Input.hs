-- | Input from the FCB1010
--
-- Intended for qualified import.
--
-- > import EurekaPROM.IO.Input qualified as Input
module EurekaPROM.IO.Input (
    Event(..)
  , Pedal(..)
  , PedalState(..)
  , Expr(..)
  , wait
  ) where

import Control.Exception

import "alsa-seq"  Sound.ALSA.Sequencer.Event qualified as Event
import "midi-alsa" Sound.MIDI.ALSA.Query ()

import EurekaPROM.IO.ALSA        qualified as ALSA
import EurekaPROM.IO.ALSA.Handle qualified as Handle
import EurekaPROM.IO.Util

import Data.MIDI qualified as MIDI

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Event =
    EventPedal Pedal PedalState
  | EventExpr  Expr  Int
  deriving stock (Show, Eq, Ord)

data Pedal =
    Pedal1
  | Pedal2
  | Pedal3
  | Pedal4
  | Pedal5
  | Pedal6
  | Pedal7
  | Pedal8
  | Pedal9
  | Pedal10
  | PedalUp
  | PedalDown
  deriving stock (Show, Eq, Ord)
  deriving Enum via AsIrregularEnum Pedal

instance Bounded Pedal where
  minBound = Pedal1
  maxBound = PedalDown

instance IrregularEnum Pedal where
  fromIrregularEnum Pedal1    = 1
  fromIrregularEnum Pedal2    = 2
  fromIrregularEnum Pedal3    = 3
  fromIrregularEnum Pedal4    = 4
  fromIrregularEnum Pedal5    = 5
  fromIrregularEnum Pedal6    = 6
  fromIrregularEnum Pedal7    = 7
  fromIrregularEnum Pedal8    = 8
  fromIrregularEnum Pedal9    = 9
  fromIrregularEnum Pedal10   = 0
  fromIrregularEnum PedalUp   = 10
  fromIrregularEnum PedalDown = 11

  toIrregularEnum 1  = Just Pedal1
  toIrregularEnum 2  = Just Pedal2
  toIrregularEnum 3  = Just Pedal3
  toIrregularEnum 4  = Just Pedal4
  toIrregularEnum 5  = Just Pedal5
  toIrregularEnum 6  = Just Pedal6
  toIrregularEnum 7  = Just Pedal7
  toIrregularEnum 8  = Just Pedal8
  toIrregularEnum 9  = Just Pedal9
  toIrregularEnum 0  = Just Pedal10
  toIrregularEnum 10 = Just PedalUp
  toIrregularEnum 11 = Just PedalDown
  toIrregularEnum _  = Nothing

  irregularSucc Pedal9  = Just Pedal10
  irregularSucc Pedal10 = Just PedalUp
  irregularSucc _       = Nothing

data Expr =
    ExprA  -- ^ Left expression pedal
  | ExprB  -- ^ Right expression pedal
  deriving stock (Show, Eq, Ord)
  deriving Enum via AsIrregularEnum Expr

instance Bounded Expr where
  minBound = ExprA
  maxBound = ExprB

instance IrregularEnum Expr where
  fromIrregularEnum ExprA = 102
  fromIrregularEnum ExprB = 103

  toIrregularEnum 102 = Just ExprA
  toIrregularEnum 103 = Just ExprB
  toIrregularEnum _   = Nothing

data PedalState = Press | Release
  deriving stock (Show, Eq, Ord)

toEvent :: MIDI.Message -> Maybe Event
toEvent msg =
    case MIDI.messageBody msg of
      MIDI.MsgControl MIDI.Control{controlNumber, controlValue} ->
        case controlNumber of
          104 ->
            EventPedal <$> toIrregularEnum controlValue <*> pure Press
          105 ->
            EventPedal <$> toIrregularEnum controlValue <*> pure Release
          _ | Just expr <- toIrregularEnum controlNumber ->
            Just $ EventExpr expr controlValue
          _otherwise ->
            Nothing
      _otherwise ->
        Nothing

{-------------------------------------------------------------------------------
  Wait for input
-------------------------------------------------------------------------------}

-- | Unexpected event
data UnexpectedEvent =
    -- | We could not parse the event as a MIDI message
    UnexpectedEvent Event.T

    -- | Unexpected MIDI message from the FCB1010
  | UnexpectedMessage MIDI.Message
  deriving stock (Show)
  deriving anyclass (Exception)

wait :: ALSA.Handle -> IO Event
wait h = do
    event <- Event.input (Handle.alsa h)
    case MIDI.convert event of
      Nothing  -> throwIO $ UnexpectedEvent event
      Just msg ->
        case toEvent msg of
          Nothing -> throwIO $ UnexpectedMessage msg
          Just ev -> return ev
