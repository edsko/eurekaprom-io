-- | System under test
--
-- Intended for qualified import.
--
-- > import Test.Mealy.SUT (SystemMonad, SystemPort)
-- > import Test.Mealy.SUT qualified as SUT
module Test.Mealy.SUT (
    -- * Monad
    SystemMonad
  , run
    -- * Initialization and termination
  , initialize
  , terminate
    -- * Interaction
  , getEvents
    -- * Command line arguments
  , SystemPort -- opaque
    -- * Re-exports
  , Input.Pedal(..)
  , Input.Event(..)
  , Input.PedalState(..)
  ) where

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Reader (ReaderT(..))
import Data.Maybe (fromMaybe)
import Test.Tasty.Options
import Test.Tasty.QuickCheck

import App.Common.Cmdline
import Control.ALSA qualified as ALSA
import Control.ALSA.Handle qualified as ALSA (Handle)
import Control.ALSA.Handle qualified as Handle
import Data.Mealy qualified as Mealy
import EurekaPROM.IO.ALSA
import EurekaPROM.IO.Input qualified as Input
import EurekaPROM.IO.Simultaneous (simultaneous)
import EurekaPROM.IO.Simultaneous qualified as Simultaneous

{-------------------------------------------------------------------------------
  Monad
-------------------------------------------------------------------------------}

type SystemMonad = ReaderT SystemState IO

data SystemState = SystemState {
      alsaHandle :: ALSA.Handle
    , deviceVar  :: MVar Simultaneous.DeviceState
    }

run :: SystemMonad a -> SystemState -> IO a
run = runReaderT

{-------------------------------------------------------------------------------
  Initialization and termination
-------------------------------------------------------------------------------}

initialize :: SystemPort -> IO SystemState
initialize NoSystemPort          = error "No port specified"
initialize (SystemPort portSpec) = do
    alsaHandle <- Handle.open
    deviceVar  <- newMVar Simultaneous.initDeviceState
    ALSA.resolve alsaHandle portSpec
    putStrLn "System initialized"
    return SystemState{alsaHandle, deviceVar}

terminate :: SystemState -> IO ()
terminate SystemState{alsaHandle, deviceVar} = do
    modifyMVar_ deviceVar $ \deviceState -> do
      case deviceState of
        Simultaneous.StateNoPedals ->
          return ()
        Simultaneous.StateOnePedal st -> do
          putStrLn $ "Please release " ++ show (Simultaneous.pedalPressed st)
          void $ waitInput alsaHandle
        Simultaneous.StateTwoPedals st -> do
          putStrLn $ "Please release " ++ show (Simultaneous.pedalPressed1 st)
          void $ waitInput alsaHandle
          putStrLn $ "Please release " ++ show (Simultaneous.pedalPressed2 st)
          void $ waitInput alsaHandle

      return Simultaneous.StateNoPedals
    Handle.close alsaHandle
    putStrLn "System terminated"

{-------------------------------------------------------------------------------
  Interaction
-------------------------------------------------------------------------------}

getEvents :: SystemMonad [Input.Event]
getEvents = ReaderT $ \SystemState{alsaHandle, deviceVar} ->
    modifyMVar deviceVar $ \deviceState -> do
      input <- waitInput alsaHandle
      return $
        fromMaybe (deviceState, []) $
          Mealy.step simultaneous (deviceState, input)

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data SystemPort =
    NoSystemPort
  | SystemPort ALSA.PortSpec

instance IsOption SystemPort where
  defaultValue   = NoSystemPort
  optionCLParser = SystemPort <$> parsePortInputOnly
  optionName     = pure "input-only"
  parseValue     = Just . SystemPort .  ALSA.PortInputOnly
  optionHelp     = pure "ALSA input port"

{-------------------------------------------------------------------------------
  QuickCheck support
-------------------------------------------------------------------------------}

instance Arbitrary Input.Pedal where
  arbitrary = elements [Input.Pedal1 .. Input.Pedal5]
