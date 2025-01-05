-- | Utilities for finding the MIDI device
--
-- Intended for qualified import.
module EurekaPROM.IO.Discovery (
    -- * Definition
    Client(..)
  , Port(..)
  , clientPortName
    -- * Discovery
  , getAllPorts
  , FindPortResult(..)
  , findPort
  ) where

import Data.List (isInfixOf)

import Sound.ALSA.Sequencer.Address     qualified as Address
import Sound.ALSA.Sequencer.Client      qualified as Client
import Sound.ALSA.Sequencer.Client.Info qualified as Client.Info
import Sound.ALSA.Sequencer.Port        qualified as Port
import Sound.ALSA.Sequencer.Port.Info   qualified as Port.Info

import EurekaPROM.IO.Alsa qualified as Alsa

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Client = Client {
      clientId   :: Client.T
    , clientName :: String
    }
  deriving (Show)

data Port = Port {
      portId   :: Port.T
    , portName :: String
    }

clientPortName :: Client -> Port -> String
clientPortName Client{clientName} Port{portName} = concat [
      clientName
    , "."
    , portName
    ]

{-------------------------------------------------------------------------------
  List all
-------------------------------------------------------------------------------}

getAllPorts :: Alsa.Handle -> IO [(Client, [Port])]
getAllPorts h@Alsa.Handle{alsa} =
    Client.Info.queryLoop alsa $ \info -> do
      client <- getClient info
      (client,) <$> getClientPorts h client

getClientPorts :: Alsa.Handle -> Client -> IO [Port]
getClientPorts Alsa.Handle{alsa} Client{clientId} =
    Port.Info.queryLoop alsa clientId $ \info ->
      getPort info

getClient :: Client.Info.T -> IO Client
getClient info =
    pure Client
      <*> Client.Info.getClient info
      <*> Client.Info.getName   info

getPort :: Port.Info.T -> IO Port
getPort info =
    pure Port
      <*> Port.Info.getPort info
      <*> Port.Info.getName info

{-------------------------------------------------------------------------------
  Find specified port
-------------------------------------------------------------------------------}

data FindPortResult =
    PortNotFound
  | PortAmbiguous [(Client, Port)]
  | PortFound Client Port Address.T

findPort :: Alsa.Handle -> String -> IO FindPortResult
findPort alsa name =
    mkResult . filter isMatch . concatMap flattenPorts <$> getAllPorts alsa
  where
    flattenPorts :: (Client, [Port]) -> [(Client, Port)]
    flattenPorts (client, ports) = map (client,) ports

    isMatch :: (Client, Port) -> Bool
    isMatch (client, port) = name `isInfixOf` clientPortName client port

    mkResult :: [(Client, Port)] -> FindPortResult
    mkResult []       = PortNotFound
    mkResult [(c, p)] = PortFound c p (mkAddress c p)
    mkResult ps       = PortAmbiguous ps

    mkAddress :: Client -> Port -> Address.T
    mkAddress c p = Address.Cons {
          client = clientId c
        , port   = portId   p
        }
