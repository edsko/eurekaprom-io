-- | Control the LEDs and the 7-segment display of the FCB1010
--
-- Reference: <https://www.eurekasound.com/eurekaprom/io>.
module EurekaPROM.IO.Output (
    -- * Definition
    LED(..)
  , Display(..)
  , Value(..)
    -- ** Auxiliary
  , Letter(..)
  , mkLetter
  , Hex(..)
  , Leading(..)
    -- * General purpose functions
  , mapDisplay
  , withDisplayValue
    -- * Toggle LEDs on the board
  , toggleLED
  , toggleDisplay
  , clearDisplay
  ) where

import Control.Monad
import Data.Char
import Data.Functor.Const
import Data.Kind

import Sound.ALSA.Sequencer.Event qualified as Event

import EurekaPROM.IO.ALSA        qualified as ALSA
import EurekaPROM.IO.ALSA.Handle qualified as Handle
import EurekaPROM.IO.ALSA.MIDI   qualified as MIDI
import EurekaPROM.IO.Util

{-------------------------------------------------------------------------------
  Preliminaries
-------------------------------------------------------------------------------}

-- | Letter (A-Z)
newtype Letter = Letter Char
  deriving stock (Show, Eq, Ord)
  deriving Enum via AsIrregularEnum Letter

mkLetter :: Char -> Maybe Letter
mkLetter c
  | 'A' <= c, c <= 'Z' = Just $ Letter c
  | ' ' == c           = Nothing
  | otherwise          = error $ "mkLetter: " ++ show c ++ " out of range"

instance IrregularEnum Letter where
  fromIrregularEnum (Letter x) = ord x - ord 'A'
  toIrregularEnum x
    | 0 <= x, x <= 25
    = Just $ Letter $ chr (ord 'A' + x)

    | otherwise
    = Nothing

instance Bounded Letter where
  minBound = Letter 'A'
  maxBound = Letter 'Z'

-- | Hexadecimal value (0-F)
newtype Hex = Hex Char
  deriving stock (Show, Eq, Ord)
  deriving Enum via AsIrregularEnum Hex

instance Bounded Hex where
  minBound = Hex '0'
  maxBound = Hex 'F'

instance IrregularEnum Hex where
  fromIrregularEnum (Hex x)
    | '0' <= x, x <= '9'
    = ord x - ord '0'

    | otherwise
    = ord x - ord 'A' + 10

  toIrregularEnum x
    | 0 <= x, x <= 9
    = Just $ Hex $ chr (ord '0' + x)

    | 10 <= x, x <= 15
    = Just $ Hex $ chr (ord 'A' + x - 10)

    | otherwise
    = Nothing

-- | Value for the leading position in the display
data Leading =
    One
  | Minus
  | Plus
  | PlusOne
  deriving stock (Show, Eq, Ord)
  deriving Enum via AsIrregularEnum Leading

instance Bounded Leading where
  minBound = One
  maxBound = PlusOne

instance IrregularEnum Leading where
  fromIrregularEnum One     = 1
  fromIrregularEnum Minus   = 2
  fromIrregularEnum Plus    = 3
  fromIrregularEnum PlusOne = 4

  toIrregularEnum 1 = Just One
  toIrregularEnum 2 = Just Minus
  toIrregularEnum 3 = Just Plus
  toIrregularEnum 4 = Just PlusOne
  toIrregularEnum _ = Nothing

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | LEDs
--
-- LEDs marked (*) are overriden by MIDI activity.
data LED =
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
  | ExprA    -- ^ (*)
  | ExprB    -- ^ (*)
  | Sw1      -- ^ (*)
  | Sw2
  | Bank
  | Select
  | Number
  | Value1
  | Value2
  | DirSel
  | MidiFn
  | MidiCh
  | Config
  deriving stock (Show, Eq, Ord)
  deriving Enum via AsIrregularEnum LED

instance Bounded LED where
  minBound = Pedal1
  maxBound = Config

instance IrregularEnum LED where
  fromIrregularEnum Pedal1  = 1
  fromIrregularEnum Pedal2  = 2
  fromIrregularEnum Pedal3  = 3
  fromIrregularEnum Pedal4  = 4
  fromIrregularEnum Pedal5  = 5
  fromIrregularEnum Pedal6  = 6
  fromIrregularEnum Pedal7  = 7
  fromIrregularEnum Pedal8  = 8
  fromIrregularEnum Pedal9  = 9
  fromIrregularEnum Pedal10 = 0
  fromIrregularEnum ExprA   = 11
  fromIrregularEnum ExprB   = 12
  fromIrregularEnum Sw1     = 13
  fromIrregularEnum Sw2     = 14
  fromIrregularEnum Bank    = 15
  fromIrregularEnum Select  = 16
  fromIrregularEnum Number  = 17
  fromIrregularEnum Value1  = 18
  fromIrregularEnum Value2  = 19
  fromIrregularEnum DirSel  = 20
  fromIrregularEnum MidiFn  = 21
  fromIrregularEnum MidiCh  = 22
  fromIrregularEnum Config  = 23

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
  toIrregularEnum 11 = Just ExprA
  toIrregularEnum 12 = Just ExprB
  toIrregularEnum 13 = Just Sw1
  toIrregularEnum 14 = Just Sw2
  toIrregularEnum 15 = Just Bank
  toIrregularEnum 16 = Just Select
  toIrregularEnum 17 = Just Number
  toIrregularEnum 18 = Just Value1
  toIrregularEnum 19 = Just Value2
  toIrregularEnum 20 = Just DirSel
  toIrregularEnum 21 = Just MidiFn
  toIrregularEnum 22 = Just MidiCh
  toIrregularEnum 23 = Just Config
  toIrregularEnum _  = Nothing

  irregularSucc Pedal9  = Just Pedal10
  irregularSucc Pedal10 = Just ExprA
  irregularSucc _       = Nothing

-- | 7-segment display
data Display :: (Type -> Type) -> Type where
  All        :: f Int     -> Display f
  TensLetter :: f Letter  -> Display f
  OnesLetter :: f Letter  -> Display f
  Leading    :: f Leading -> Display f
  TensHex    :: f Hex     -> Display f
  OnesHex    :: f Hex     -> Display f

deriving stock instance Show a => Show (Display (Const a))
deriving stock instance Eq   a => Eq   (Display (Const a))
deriving stock instance Ord  a => Ord  (Display (Const a))

deriving stock instance Show (Display Value)
deriving stock instance Eq   (Display Value)
deriving stock instance Ord  (Display Value)

instance Bounded (Display (Const ())) where
  minBound = All     (Const ())
  maxBound = OnesHex (Const ())

deriving
  via AsIrregularEnum (Display (Const ()))
  instance Enum (Display (Const ()))

instance IrregularEnum (Display (Const ())) where
  fromIrregularEnum (All        (Const ())) = 108
  fromIrregularEnum (TensLetter (Const ())) = 109
  fromIrregularEnum (OnesLetter (Const ())) = 110
  fromIrregularEnum (Leading    (Const ())) = 112
  fromIrregularEnum (TensHex    (Const ())) = 113
  fromIrregularEnum (OnesHex    (Const ())) = 114

  toIrregularEnum 108 = Just $ All        (Const ())
  toIrregularEnum 109 = Just $ TensLetter (Const ())
  toIrregularEnum 110 = Just $ OnesLetter (Const ())
  toIrregularEnum 112 = Just $ Leading    (Const ())
  toIrregularEnum 113 = Just $ TensHex    (Const ())
  toIrregularEnum 114 = Just $ OnesHex    (Const ())
  toIrregularEnum _   = Nothing

  irregularSucc (OnesLetter (Const ())) = Just $ Leading (Const ())
  irregularSucc _                       = Nothing

data Value :: Type -> Type where
  ValueInt     :: Int           -> Value Int
  ValueLetter  :: Maybe Letter  -> Value Letter
  ValueLeading :: Maybe Leading -> Value Leading
  ValueHex     :: Maybe Hex     -> Value Hex

deriving instance Show (Value a)
deriving instance Eq   (Value a)
deriving instance Ord  (Value a)

{-------------------------------------------------------------------------------
  General purpose functions
-------------------------------------------------------------------------------}

mapDisplay :: (forall a. f a -> g a) -> Display f -> Display g
mapDisplay f (All        x) = All        (f x)
mapDisplay f (TensLetter x) = TensLetter (f x)
mapDisplay f (OnesLetter x) = OnesLetter (f x)
mapDisplay f (Leading    x) = Leading    (f x)
mapDisplay f (TensHex    x) = TensHex    (f x)
mapDisplay f (OnesHex    x) = OnesHex    (f x)

withDisplayValue :: (forall a. Value a -> r) -> Display Value -> r
withDisplayValue k (All        x) = k x
withDisplayValue k (TensLetter x) = k x
withDisplayValue k (OnesLetter x) = k x
withDisplayValue k (Leading    x) = k x
withDisplayValue k (TensHex    x) = k x
withDisplayValue k (OnesHex    x) = k x

{-------------------------------------------------------------------------------
  Translate to MIDI message
-------------------------------------------------------------------------------}

ledToMIDI :: LED -> Bool -> MIDI.Message
ledToMIDI led val = MIDI.Message {
      msgChannel = 0
    , msgBody    = MIDI.MsgControl MIDI.Control {
          controlNumber = if val then 106 else 107
        , controlValue  = fromEnum led
        }
    }

valueToMIDI :: Value a -> Int
valueToMIDI (ValueInt     x)        = x
valueToMIDI (ValueLetter  (Just x)) = fromEnum x
valueToMIDI (ValueLetter  Nothing)  = 26
valueToMIDI (ValueLeading (Just x)) = fromEnum x
valueToMIDI (ValueLeading Nothing)  = 5
valueToMIDI (ValueHex     (Just x)) = fromEnum x
valueToMIDI (ValueHex     Nothing)  = 16

displayToMIDI :: Display Value -> MIDI.Message
displayToMIDI val = MIDI.Message {
      msgChannel = 0
    , msgBody    = MIDI.MsgControl MIDI.Control {
          controlNumber = fromEnum $ mapDisplay (\_ -> Const ()) val
        , controlValue  = withDisplayValue valueToMIDI val
        }
    }

{-------------------------------------------------------------------------------
  Send MIDI messages
-------------------------------------------------------------------------------}

toggleLED :: ALSA.Handle -> LED -> Bool -> IO ()
toggleLED h led val = do
    let ev = Event.simple (Handle.address h) eventData
    void $ Event.outputDirect (Handle.alsa h) ev
  where
    eventData :: Event.Data
    eventData = MIDI.toALSA $ ledToMIDI led val

toggleDisplay :: ALSA.Handle -> Display Value -> IO ()
toggleDisplay h val = do
    let ev = Event.simple (Handle.address h) eventData
    void $ Event.outputDirect (Handle.alsa h) ev
  where
    eventData :: Event.Data
    eventData = MIDI.toALSA $ displayToMIDI val

clearDisplay :: ALSA.Handle -> IO ()
clearDisplay h = do
    toggleDisplay h $ Leading    $ ValueLeading Nothing
    toggleDisplay h $ TensLetter $ ValueLetter  Nothing
    toggleDisplay h $ OnesLetter $ ValueLetter  Nothing