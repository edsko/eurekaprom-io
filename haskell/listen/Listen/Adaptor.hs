-- | Monad for turning MIDI messages into input device events
--
-- Intended for qualified import.
module Listen.Adaptor (
    Adaptor -- opaque
  , run
    -- * State
  , deltaCC
    -- * Input events
  , InputEvent(..)
  , writeInputEvents
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.State (MonadState(..), StateT(..), evalStateT)
import Data.Map (Map)
import Data.Map qualified as Map

import Evdev        qualified
import Evdev.Codes  qualified as Codes
import Evdev.Uinput qualified as Uinput

import EurekaPROM.IO.MIDI qualified as MIDI

{-------------------------------------------------------------------------------
  Definition

  NOTE: 'AdaptorEnv' and 'AdaptorState' are both not exported.
-------------------------------------------------------------------------------}

-- | Monad for turning MIDI messages into input device events
newtype Adaptor a = WrapAdaptor {
      unwrapAdaptor :: StateT AdaptorState (ReaderT AdaptorEnv IO) a
    }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

data AdaptorEnv = AdaptorEnv {
      envUInput :: Uinput.Device
    }

data AdaptorState = AdaptorState {
      stCC :: Map Int Int
    }

initAdaptorState :: AdaptorState
initAdaptorState = AdaptorState {
      stCC = Map.empty
    }

run :: Adaptor a -> IO a
run adaptor = do
    envUInput <- Uinput.newDevice "eurekaprom-io-listen" deviceOpts

    let env :: AdaptorEnv
        env = AdaptorEnv{
              envUInput
            }

    flip runReaderT env $ evalStateT (unwrapAdaptor adaptor) initAdaptorState
  where
    deviceOpts :: Uinput.DeviceOpts
    deviceOpts = Uinput.defaultDeviceOpts{
          Uinput.keys = concat [
              -- [KeyA .. KeyZ] does /not/ work (poor 'Enum' instance)
              [ Codes.KeyA
              , Codes.KeyB
              , Codes.KeyC
              , Codes.KeyD
              , Codes.KeyE
              , Codes.KeyF
              , Codes.KeyG
              , Codes.KeyH
              , Codes.KeyI
              , Codes.KeyJ
              , Codes.KeyK
              , Codes.KeyL
              , Codes.KeyM
              , Codes.KeyN
              , Codes.KeyO
              , Codes.KeyP
              , Codes.KeyQ
              , Codes.KeyR
              , Codes.KeyS
              , Codes.KeyT
              , Codes.KeyU
              , Codes.KeyV
              , Codes.KeyW
              , Codes.KeyX
              , Codes.KeyY
              , Codes.KeyA
              ]

              -- [BtnLeft] must be enabled for /any/ mouse events to be allowed
            , [Codes.BtnLeft, Codes.BtnRight]
            ]
        , Uinput.relAxes = [Codes.RelX, Codes.RelY]
        }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

askEnv :: (AdaptorEnv -> a) -> Adaptor a
askEnv f = WrapAdaptor $ f <$> ask

withUInput :: (Uinput.Device -> Adaptor a) -> Adaptor a
withUInput f = askEnv envUInput >>= f

writeBatch :: [Evdev.EventData] -> Adaptor ()
writeBatch es = withUInput $ \device -> liftIO $ Uinput.writeBatch device es

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

deltaCC :: MIDI.Control -> Adaptor (Maybe Int)
deltaCC MIDI.Control{controlNumber, controlValue = newValue} = WrapAdaptor $
    state $ \st -> (
        do oldValue <- Map.lookup controlNumber (stCC st)
           return $ newValue - oldValue
      , st{stCC = Map.insert controlNumber newValue (stCC st)}
      )

{-------------------------------------------------------------------------------
  Input events
-------------------------------------------------------------------------------}

data InputEvent =
    -- | Press and release the specified key
    Key Codes.Key

    -- | Move the mouse
  | Rel (Int, Int)

fromInputEvent :: InputEvent -> [Evdev.EventData]
fromInputEvent (Key key) = [
      Evdev.KeyEvent key Evdev.Pressed
    , Evdev.KeyEvent key Evdev.Released
    ]
fromInputEvent (Rel (x, y)) = [
      Evdev.RelativeEvent Codes.RelX (toEnum x)
    , Evdev.RelativeEvent Codes.RelY (toEnum y)
    ]

writeInputEvents :: [InputEvent] -> Adaptor ()
writeInputEvents = writeBatch . concatMap fromInputEvent