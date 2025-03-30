-- | Input from the FCB1010
--
-- Intended for qualified import.
--
-- > import EurekaPROM.IO.Input qualified as Input
module EurekaPROM.IO.Input (
    -- * Definition
    Event(..)
  , Pedal(..)
  , PedalState(..)
  , Expr(..)
    -- * MIDI
  , fromMIDI
  , toMIDI
  ) where

import Data.MIDI qualified as MIDI

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data PedalState = Press | Release
  deriving stock (Show, Eq, Ord, Bounded, Enum)

data Pedal =
    Pedal10
  | Pedal1
  | Pedal2
  | Pedal3
  | Pedal4
  | Pedal5
  | Pedal6
  | Pedal7
  | Pedal8
  | Pedal9
  | PedalUp
  | PedalDown
  deriving stock (Show, Eq, Ord, Bounded)

instance Enum Pedal where
  fromEnum Pedal10   = 0
  fromEnum Pedal1    = 1
  fromEnum Pedal2    = 2
  fromEnum Pedal3    = 3
  fromEnum Pedal4    = 4
  fromEnum Pedal5    = 5
  fromEnum Pedal6    = 6
  fromEnum Pedal7    = 7
  fromEnum Pedal8    = 8
  fromEnum Pedal9    = 9
  fromEnum PedalUp   = 10
  fromEnum PedalDown = 11

  toEnum 0  = Pedal10
  toEnum 1  = Pedal1
  toEnum 2  = Pedal2
  toEnum 3  = Pedal3
  toEnum 4  = Pedal4
  toEnum 5  = Pedal5
  toEnum 6  = Pedal6
  toEnum 7  = Pedal7
  toEnum 8  = Pedal8
  toEnum 9  = Pedal9
  toEnum 10 = PedalUp
  toEnum 11 = PedalDown
  toEnum n  = error $ "(toEnum " ++ show n ++ " :: Pedal) not defined"

data Expr =
    ExprA  -- ^ Left expression pedal
  | ExprB  -- ^ Right expression pedal
  deriving stock (Show, Eq, Ord, Bounded)

instance Enum Expr where
  fromEnum ExprA = 102
  fromEnum ExprB = 103

  toEnum 102 = ExprA
  toEnum 103 = ExprB
  toEnum n   = error $ "(toEnum " ++ show n ++ " :: Expr) not defined"

data Event =
    EventPedal Pedal PedalState
  | EventExpr  Expr  Int
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  MIDI
-------------------------------------------------------------------------------}

fromMIDI :: MIDI.Message -> Maybe Event
fromMIDI msg =
    case MIDI.messageBody msg of
      MIDI.MsgControl MIDI.Control{controlNumber = num, controlValue = val} ->
        if | num == 104 ->
              Just $ EventPedal (toEnum val) Press
           | num == 105 ->
              Just $ EventPedal (toEnum val) Release
           | num == fromEnum ExprA || num == fromEnum ExprB ->
              Just $ EventExpr (toEnum num) val
           | otherwise ->
              Nothing
      _otherwise ->
        Nothing

toMIDI :: Event -> MIDI.Message
toMIDI event = MIDI.Message {
      messageChannel = 0
    , messageBody    = MIDI.MsgControl $
        case event of
          EventPedal pedal state -> MIDI.Control {
              controlNumber =
                case state of
                  Press   -> 104
                  Release -> 105
            , controlValue = fromEnum pedal
            }
          EventExpr expr value -> MIDI.Control {
              controlNumber = fromEnum expr
            , controlValue  = value
            }
    }
