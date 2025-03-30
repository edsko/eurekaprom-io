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
  , showInstruction
  , getEvents
  , delay
    -- * Command line arguments
  , SystemPort -- opaque
  ) where

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT(..))
import Data.Maybe (fromMaybe)
import System.Console.ANSI
import System.Timeout
import Test.Tasty.Options

import App.Common.Cmdline
import Control.ALSA qualified as ALSA
import Control.ALSA.Handle qualified as ALSA (Handle)
import Control.ALSA.Handle qualified as Handle
import Data.Mealy qualified as Mealy
import EurekaPROM.IO.ALSA
import EurekaPROM.IO.Input
import EurekaPROM.IO.Simultaneous (simultaneous)
import EurekaPROM.IO.Simultaneous qualified as Simultaneous

import Test.Mealy.Model (PedalAction(..))

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
    dropInput alsaHandle
    putStrLn "System initialized"
    return SystemState{alsaHandle, deviceVar}

terminate :: SystemState -> IO ()
terminate SystemState{alsaHandle, deviceVar} = do
    modifyMVar_ deviceVar $ \deviceState -> do
      case deviceState of
        Simultaneous.StateNoPedals ->
          return ()
        _otherwise -> do
          setSGR [SetColor Foreground Dull Red]
          putStrLn "Please release all pedals."
          setSGR [Reset]
          delay
          dropInput alsaHandle
      return Simultaneous.StateNoPedals
    Handle.close alsaHandle
    putStrLn "System terminated"

{-------------------------------------------------------------------------------
  Interaction
-------------------------------------------------------------------------------}

getEvents :: SystemMonad [Event]
getEvents = ReaderT $ \SystemState{alsaHandle, deviceVar} ->
    modifyMVar deviceVar $ \deviceState -> do
      input <- waitInput alsaHandle
      return $
        fromMaybe (deviceState, []) $
          Mealy.step simultaneous (deviceState, input)

showInstruction :: MonadIO m => PedalAction -> m ()
showInstruction action = liftIO $ do
    case action of
      ActPress pedal -> do
        setSGR [SetColor Foreground Dull Green]
        putStrLn $ "Please press " ++ show pedal
      ActRelease pedal -> do
        setSGR [SetColor Foreground Dull Red]
        putStrLn $ "Please release " ++ show pedal
    setSGR [Reset]

delay :: MonadIO m => m ()
delay = liftIO $ do
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn "Continuing after delay or key press."
    setSGR [Reset]
    void $ timeout waitTime $ getLine
  where
    waitTime :: Int
    waitTime = 5_000_000

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
