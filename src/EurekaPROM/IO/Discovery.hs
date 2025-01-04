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

import Sound.ALSA.Sequencer             qualified as Alsa
import Sound.ALSA.Sequencer.Client      qualified as Client
import Sound.ALSA.Sequencer.Port.Info   qualified as Port.Info
import Sound.ALSA.Sequencer.Client.Info qualified as Client.Info
import Sound.ALSA.Sequencer.Port qualified as Port

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

getAllPorts :: Alsa.T mode -> IO [(Client, [Port])]
getAllPorts alsa =
    Client.Info.queryLoop alsa $ \info -> do
      client <- getClient info
      (client,) <$> getClientPorts alsa client

getClientPorts :: Alsa.T mode -> Client -> IO [Port]
getClientPorts alsa Client{clientId} =
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
  | PortFound Client Port

findPort :: Alsa.T mode -> String -> IO FindPortResult
findPort alsa name =
    mkResult . filter isMatch . concatMap flattenPorts <$> getAllPorts alsa
  where
    flattenPorts :: (Client, [Port]) -> [(Client, Port)]
    flattenPorts (client, ports) = map (client,) ports

    isMatch :: (Client, Port) -> Bool
    isMatch (client, port) = name `isInfixOf` clientPortName client port

    mkResult :: [(Client, Port)] -> FindPortResult
    mkResult []       = PortNotFound
    mkResult [(c, p)] = PortFound c p
    mkResult ps       = PortAmbiguous ps
