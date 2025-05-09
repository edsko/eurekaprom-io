-- | Interact with the EurekaPROM
--
-- Intended for unqualified import.
module EurekaPROM.IO.ALSA (
    -- * Initialization
    ALSA.Handle
  , ALSA.PortSpec(..)
  , ALSA.getPortNames
  , ALSA.resolve
  , Handle.open
  , Handle.close
    -- * Input
  , waitInput
  , waitInputUsing
  , dropInput
    -- * Output
  , toggleLED
  , toggleDisplay
  , clearDisplay
  ) where

import Control.Monad
import Control.Exception

import Control.ALSA qualified as ALSA
import Control.ALSA.Event qualified as ALSA.Event
import Control.ALSA.Handle qualified as ALSA (Handle)
import Control.ALSA.Handle qualified as Handle
import Data.MIDI qualified as MIDI

import EurekaPROM.IO.Input qualified as Input
import EurekaPROM.IO.Output qualified as Output

{-------------------------------------------------------------------------------
  Wait for input
-------------------------------------------------------------------------------}

-- | Unexpected event
data UnexpectedEvent =
    -- | We could not parse the event as a MIDI message
    UnexpectedEvent ALSA.Event.T

    -- | Unexpected MIDI message from the FCB1010
  | UnexpectedMessage MIDI.Message
  deriving stock (Show)
  deriving anyclass (Exception)

waitInput :: ALSA.Handle -> IO Input.Event
waitInput = waitInputUsing Input.eventFromMIDI

waitInputUsing :: (MIDI.Message -> Maybe a) -> ALSA.Handle -> IO a
waitInputUsing parser h = do
    event <- ALSA.Event.input (Handle.alsa h)
    case MIDI.convert event of
      Nothing  -> throwIO $ UnexpectedEvent event
      Just msg ->
        case parser msg of
          Nothing -> throwIO $ UnexpectedMessage msg
          Just ev -> return ev

dropInput :: ALSA.Handle -> IO ()
dropInput h = ALSA.Event.dropInput (Handle.alsa h)

{-------------------------------------------------------------------------------
  Send MIDI messages
-------------------------------------------------------------------------------}

toggleLED :: ALSA.Handle -> Output.LED -> Bool -> IO ()
toggleLED h led val = do
    let ev = ALSA.Event.simple (Handle.address h) eventData
    void $ ALSA.Event.outputDirect (Handle.alsa h) ev
  where
    eventData :: ALSA.Event.Data
    eventData = MIDI.convert' $ Output.ledToMIDI led val

toggleDisplay :: ALSA.Handle -> Output.Display Output.Value -> IO ()
toggleDisplay h val = do
    let ev = ALSA.Event.simple (Handle.address h) eventData
    void $ ALSA.Event.outputDirect (Handle.alsa h) ev
  where
    eventData :: ALSA.Event.Data
    eventData = MIDI.convert' $ Output.displayToMIDI val

clearDisplay :: ALSA.Handle -> IO ()
clearDisplay h = do
    toggleDisplay h $ Output.Leading    $ Output.ValueLeading Nothing
    toggleDisplay h $ Output.TensLetter $ Output.ValueLetter  Nothing
    toggleDisplay h $ Output.OnesLetter $ Output.ValueLetter  Nothing