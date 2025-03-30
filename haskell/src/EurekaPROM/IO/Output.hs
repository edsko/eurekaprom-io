-- | Control the LEDs and the 7-segment display of the FCB1010
--
-- Reference: <https://www.eurekasound.com/eurekaprom/io>
--
-- Intended for qualified import.
--
-- > import EurekaPROM.IO.Output qualified as Output
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
    -- * MIDI
  , ledToMIDI
  , valueToMIDI
  , displayToMIDI
  ) where

import Data.Char (ord, chr)
import Data.Functor.Const
import Data.Kind

import Data.MIDI qualified as MIDI

{-------------------------------------------------------------------------------
  Letters
-------------------------------------------------------------------------------}

-- | Letter (A-Z)
newtype Letter = Letter Char
  deriving stock (Show, Eq, Ord)

instance Bounded Letter where
  minBound = Letter 'A'
  maxBound = Letter 'Z'

instance Enum Letter where
  fromEnum (Letter x) = ord x - ord 'A'
  toEnum n
    | 0 <= n, n <= 25
    = Letter $ chr (ord 'A' + n)

    | otherwise
    = error $ "(toEnum " ++ show n ++ " :: Letter) not defined"

mkLetter :: Char -> Maybe Letter
mkLetter c
  | 'A' <= c, c <= 'Z' = Just $ Letter c
  | ' ' == c           = Nothing
  | otherwise          = error $ "mkLetter: " ++ show c ++ " out of range"

{-------------------------------------------------------------------------------
  Hexadecimal values
-------------------------------------------------------------------------------}

-- | Hexadecimal value (0-F)
newtype Hex = Hex Char
  deriving stock (Show, Eq, Ord)

instance Bounded Hex where
  minBound = Hex '0'
  maxBound = Hex 'F'

instance Enum Hex where
  fromEnum (Hex x)
    | '0' <= x, x <= '9'
    = ord x - ord '0'

    | otherwise
    = ord x - ord 'A' + 10

  toEnum n
    | 0 <= n, n <= 9
    = Hex $ chr (ord '0' + n)

    | 10 <= n, n <= 15
    = Hex $ chr (ord 'A' + n - 10)

    | otherwise
    = error $ "(toEnum " ++ show n ++ " :: Hex) not defined"

{-------------------------------------------------------------------------------
  Value for the leading position in the display
-------------------------------------------------------------------------------}

-- | Value for the leading position in the display
data Leading =
    One
  | Minus
  | Plus
  | PlusOne
  deriving stock (Show, Eq, Ord)

instance Bounded Leading where
  minBound = One
  maxBound = PlusOne

instance Enum Leading where
  fromEnum One     = 1
  fromEnum Minus   = 2
  fromEnum Plus    = 3
  fromEnum PlusOne = 4

  toEnum 1 = One
  toEnum 2 = Minus
  toEnum 3 = Plus
  toEnum 4 = PlusOne
  toEnum n = error $ "(toEnum " ++ show n ++ " :: Leading) not defined"

{-------------------------------------------------------------------------------
  Value (any of the above)
-------------------------------------------------------------------------------}

data Value :: Type -> Type where
  ValueInt     :: Int           -> Value Int
  ValueLetter  :: Maybe Letter  -> Value Letter
  ValueLeading :: Maybe Leading -> Value Leading
  ValueHex     :: Maybe Hex     -> Value Hex

deriving instance Show (Value a)
deriving instance Eq   (Value a)
deriving instance Ord  (Value a)

{-------------------------------------------------------------------------------
  LED
-------------------------------------------------------------------------------}

-- | LEDs
--
-- LEDs marked (*) are overriden by MIDI activity.
--
-- NOTE: 'Enum' instance is derived, and not related to MIDI values
-- (MIDI values are not consecutive).
data LED =
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
  deriving stock (Show, Eq, Ord, Bounded, Enum)

{-------------------------------------------------------------------------------
  Full display
-------------------------------------------------------------------------------}

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
      messageChannel = 0
    , messageBody    = MIDI.MsgControl MIDI.Control {
          controlNumber = if val then 106 else 107
        , controlValue  = aux led
        }
    }
  where
    aux Pedal10 = 0
    aux Pedal1  = 1
    aux Pedal2  = 2
    aux Pedal3  = 3
    aux Pedal4  = 4
    aux Pedal5  = 5
    aux Pedal6  = 6
    aux Pedal7  = 7
    aux Pedal8  = 8
    aux Pedal9  = 9
    -- NOTE: Gap at 10
    aux ExprA   = 11
    aux ExprB   = 12
    aux Sw1     = 13
    aux Sw2     = 14
    aux Bank    = 15
    aux Select  = 16
    aux Number  = 17
    aux Value1  = 18
    aux Value2  = 19
    aux DirSel  = 20
    aux MidiFn  = 21
    aux MidiCh  = 22
    aux Config  = 23

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
      messageChannel = 0
    , messageBody    = MIDI.MsgControl MIDI.Control {
          controlNumber = aux $ mapDisplay (\_ -> Const ()) val
        , controlValue  = withDisplayValue valueToMIDI val
        }
    }
  where
    aux :: Display (Const ()) -> Int
    aux (All        (Const ())) = 108
    aux (TensLetter (Const ())) = 109
    aux (OnesLetter (Const ())) = 110
    -- NOTE: Gap at 111
    aux (Leading    (Const ())) = 112
    aux (TensHex    (Const ())) = 113
    aux (OnesHex    (Const ())) = 114
