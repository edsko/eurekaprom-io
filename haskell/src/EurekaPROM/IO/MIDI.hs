{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Interface between @alsa-seq@ and @midi@, using @midi-alsa@
--
-- Intended for qualified import.
module EurekaPROM.IO.MIDI (
    Message(..)
  , MessageBody(..)
    -- * Conversion
  , fromALSA
  ) where

import Optics

import Sound.MIDI.ALSA.Query     ()
import Sound.MIDI.ALSA.Construct ()

import Sound.MIDI.Message.Channel         qualified as MIDI (Channel)
import Sound.MIDI.Message.Channel.Mode    qualified as MIDI.Mode
import Sound.MIDI.Message.Channel.Voice   qualified as MIDI.Voice
import Sound.MIDI.Message.Class.Construct qualified as MIDI.Construct
import Sound.MIDI.Message.Class.Query     qualified as MIDI.Query

import Sound.ALSA.Sequencer.Event qualified as ALSA.Event

{-------------------------------------------------------------------------------
  More user-friendly datatype
-------------------------------------------------------------------------------}

data Message = Message {
      messageChannel :: MIDI.Channel
    , messageBody    :: MessageBody
    }
  deriving (Show)

data MessageBody =
    Note MIDI.Voice.Velocity MIDI.Voice.Pitch Bool
  | Program MIDI.Voice.Program
  | AnyController MIDI.Voice.Controller Int
  | PitchBend Int
  | ChannelPressure Int
  | Mode MIDI.Mode.T
  deriving (Show)

makePrismLabels ''MessageBody

instance MIDI.Query.C Message where
  note            = queryWith #_Note
  program         = queryWith #_Program
  anyController   = queryWith #_AnyController
  pitchBend       = queryWith #_PitchBend
  channelPressure = queryWith #_ChannelPressure
  mode            = queryWith #_Mode

instance MIDI.Construct.C Message where
  note            = constructWith #_Note
  program         = constructWith #_Program
  anyController   = constructWith #_AnyController
  pitchBend       = constructWith #_PitchBend
  channelPressure = constructWith #_ChannelPressure
  mode            = constructWith #_Mode

queryWith :: Prism' MessageBody x -> Message -> Maybe (MIDI.Channel, x)
queryWith p Message{messageChannel, messageBody} =
    (messageChannel,) <$> preview p messageBody

constructWith :: Prism' MessageBody x -> MIDI.Channel -> x -> Message
constructWith p messageChannel x = Message{
      messageChannel
    , messageBody = review p x
    }

{-------------------------------------------------------------------------------
  Conversion utilities
-------------------------------------------------------------------------------}

fromALSA :: ALSA.Event.T -> Maybe Message
fromALSA = convert

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
