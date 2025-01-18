{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Interface between @alsa-seq@ and @midi@, using @midi-alsa@
--
-- Intended for qualified import.
module EurekaPROM.IO.ALSA.MIDI (
    -- * Messages
    Message(..)
  , MessageBody(..)
  , Note(..)
  , Control(..)
    -- * Conversion
  , fromALSA
  , toALSA
  ) where

import Data.Kind
import Data.Maybe (fromMaybe)
import Optics

import Sound.MIDI.ALSA.Query     ()
import Sound.MIDI.ALSA.Construct ()

import Sound.MIDI.Message.Channel         qualified as MIDI (Channel)
import Sound.MIDI.Message.Channel         qualified as MIDI.Channel
import Sound.MIDI.Message.Channel.Mode    qualified as MIDI.Mode
import Sound.MIDI.Message.Channel.Voice   qualified as MIDI.Voice
import Sound.MIDI.Message.Class.Construct qualified as MIDI.Construct
import Sound.MIDI.Message.Class.Query     qualified as MIDI.Query

import Sound.ALSA.Sequencer.Event qualified as ALSA.Event

{-------------------------------------------------------------------------------
  More user-friendly datatype
-------------------------------------------------------------------------------}

data Message = Message {
      msgChannel :: Int
    , msgBody    :: MessageBody
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

makePrismLabels ''MessageBody
makePrismLabels ''Note
makePrismLabels ''Control

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
queryWith p Message{msgChannel, msgBody} =
    (MIDI.Channel.toChannel msgChannel,) <$>
      preview (p % re wrapped) msgBody

constructWith ::
     Wrapped x
  => Prism' MessageBody (Unwrapped x) -> MIDI.Channel -> x -> Message
constructWith p channel x = Message{
      msgChannel = MIDI.Channel.fromChannel channel
    , msgBody    = review (p % re wrapped) x
    }

{-------------------------------------------------------------------------------
  Conversion utilities
-------------------------------------------------------------------------------}

fromALSA :: ALSA.Event.T -> Maybe Message
fromALSA = convert

toALSA :: Message -> ALSA.Event.Data
toALSA msg =
    fromMaybe (error $ "toALSA: could not convert " ++ show msg) $
      convert msg

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Wrapping and unwrapping MIDI types
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
