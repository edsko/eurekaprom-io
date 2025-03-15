{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Idiomatic Haskell definition of MIDI messages
--
-- Intended for use with the @midi@ package.
--
-- Note: this module does /not/ have any ALSA specific functionality.
--
-- Intended for qualified import.
--
-- > import Data.MIDI qualified as MIDI
module Data.MIDI (
    -- * Definition
    Message(..)
  , MessageBody(..)
  , Note(..)
  , Control(..)
    -- * Utility
  , convert
  , convert'
  ) where

import Data.Kind
import Data.Maybe (fromMaybe)
import GHC.Stack
import Optics

-- We import from the @midi@ package here, but /not/ from ALSA.
import "midi" Sound.MIDI.Message.Channel         qualified as MIDI (Channel)
import "midi" Sound.MIDI.Message.Channel         qualified as MIDI.Channel
import "midi" Sound.MIDI.Message.Channel.Mode    qualified as MIDI.Mode
import "midi" Sound.MIDI.Message.Channel.Voice   qualified as MIDI.Voice
import "midi" Sound.MIDI.Message.Class.Construct qualified as MIDI.Construct
import "midi" Sound.MIDI.Message.Class.Query     qualified as MIDI.Query

{-------------------------------------------------------------------------------
  Definition

  TODO: We should reify the MIDI control channels.
  <https://midi.org/midi-1-0-control-change-messages>
-------------------------------------------------------------------------------}

data Message = Message {
      messageChannel :: Int
    , messageBody    :: MessageBody
    }
  deriving stock (Show)

data MessageBody =
    MsgNote Note
  | MsgProgram Int
  | MsgControl Control
  | MsgPitchBend Int
  | MsgChannelPressure Int
  | MsgMode MIDI.Mode.T
  deriving stock (Show)

data Note = Note {
      noteVelocity :: Int
    , notePitch    :: Int
    , noteOn       :: Bool
    }
  deriving stock (Show)

data Control = Control {
      controlNumber :: Int
    , controlValue  :: Int
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Lenses and prisms

  Examples:

  > #body     :: Lens' Message MessageBody
  > #_MsgNote :: Prism' MessageBody Note
-------------------------------------------------------------------------------}

makeFieldLabels ''Message
makeFieldLabels ''Note
makeFieldLabels ''Control

makePrismLabels ''Message
makePrismLabels ''MessageBody
makePrismLabels ''Note
makePrismLabels ''Control

{-------------------------------------------------------------------------------
  Interop with @midi@
-------------------------------------------------------------------------------}

instance MIDI.Query.C Message where
  note            = queryWith $ #_MsgNote % #_Note
  program         = queryWith $ #_MsgProgram
  anyController   = queryWith $ #_MsgControl % #_Control
  pitchBend       = queryWith $ #_MsgPitchBend
  channelPressure = queryWith $ #_MsgChannelPressure
  mode            = queryWith $ #_MsgMode

instance MIDI.Construct.C Message where
  note            = constructWith $ #_MsgNote % #_Note
  program         = constructWith $ #_MsgProgram
  anyController   = constructWith $ #_MsgControl % #_Control
  pitchBend       = constructWith $ #_MsgPitchBend
  channelPressure = constructWith $ #_MsgChannelPressure
  mode            = constructWith $ #_MsgMode

queryWith ::
     Wrapped x
  => Prism' MessageBody (Unwrapped x) -> Message -> Maybe (MIDI.Channel, x)
queryWith p Message{messageChannel, messageBody} =
    (MIDI.Channel.toChannel messageChannel,) <$>
      preview (p % re wrapped) messageBody

constructWith ::
     Wrapped x
  => Prism' MessageBody (Unwrapped x) -> MIDI.Channel -> x -> Message
constructWith p channel x = Message{
      messageChannel = MIDI.Channel.fromChannel channel
    , messageBody    = review (p % re wrapped) x
    }

{-------------------------------------------------------------------------------
  Internal: Wrapping and unwrapping MIDI types
-------------------------------------------------------------------------------}

class Wrapped a where
  type Unwrapped a :: Type
  type Unwrapped a = a

  wrapped :: Iso' a (Unwrapped a)
  default wrapped :: Unwrapped a ~ a => Iso' a (Unwrapped a)
  wrapped = simple

instance Wrapped Bool
instance Wrapped Int
instance Wrapped MIDI.Mode.T

instance Wrapped MIDI.Channel where
  type Unwrapped MIDI.Channel = Int
  wrapped = iso MIDI.Channel.fromChannel MIDI.Channel.toChannel

instance Wrapped MIDI.Voice.Velocity where
  type Unwrapped MIDI.Voice.Velocity = Int
  wrapped = iso MIDI.Voice.fromVelocity MIDI.Voice.toVelocity

instance Wrapped MIDI.Voice.Pitch where
  type Unwrapped MIDI.Voice.Pitch = Int
  wrapped = iso MIDI.Voice.fromPitch MIDI.Voice.toPitch

instance Wrapped MIDI.Voice.Program where
  type Unwrapped MIDI.Voice.Program = Int
  wrapped = iso MIDI.Voice.fromProgram MIDI.Voice.toProgram

instance Wrapped MIDI.Voice.Controller where
  type Unwrapped MIDI.Voice.Controller = Int
  wrapped = iso MIDI.Voice.fromController MIDI.Voice.toController

instance (Wrapped a, Wrapped b) => Wrapped (a, b) where
  type Unwrapped (a, b) = (Unwrapped a, Unwrapped b)
  wrapped =
      iso (aux     wrapped      wrapped )
          (aux (re wrapped) (re wrapped))
    where
      aux i1 i2 (a, b) = (view i1 a, view i2 b)

instance (Wrapped a, Wrapped b, Wrapped c) => Wrapped (a, b, c) where
  type Unwrapped (a, b, c) = (Unwrapped a, Unwrapped b, Unwrapped c)
  wrapped =
      iso (aux     wrapped      wrapped      wrapped )
          (aux (re wrapped) (re wrapped) (re wrapped))
    where
      aux i1 i2 i3 (a, b, c) = (view i1 a, view i2 b, view i3 c)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Convert between MIDI representations
convert :: (MIDI.Query.C a, MIDI.Construct.C b) => a -> Maybe b
convert a
  | Just x <- MIDI.Query.note a
  = Just $ uncurry MIDI.Construct.note x

  | Just x <- MIDI.Query.program a
  = Just $ uncurry MIDI.Construct.program x

  | Just x <- MIDI.Query.anyController a
  = Just $ uncurry MIDI.Construct.anyController x

  | Just x <- MIDI.Query.pitchBend a
  = Just $ uncurry MIDI.Construct.pitchBend x

  | Just x <- MIDI.Query.channelPressure a
  = Just $ uncurry MIDI.Construct.channelPressure x

  | Just x <- MIDI.Query.mode a
  = Just $ uncurry MIDI.Construct.mode x

  | otherwise
  = Nothing

-- | Like 'convert', but throw pure exception when conversion fails
convert' :: (HasCallStack, MIDI.Query.C a, MIDI.Construct.C b) => a -> b
convert' = fromMaybe (error "conversion failed") . convert
