-- | Utilities for finding the MIDI device
--
-- Intended for qualified import.
module EurekaPROM.IO.ALSA.Discovery (
    -- * Definition
    Client(..)
  , Port(..)
  , portQualifiedName
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

import EurekaPROM.IO.ALSA.Handle qualified as ALSA (Handle)
import EurekaPROM.IO.ALSA.Handle qualified as Handle

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Client = Client {
      clientId   :: Client.T
    , clientName :: String
    }
  deriving (Show)

data Port = Port {
      portClient  :: Client
    , portId      :: Port.T
    , portName    :: String
    , portAddress :: Address.T
    }

portQualifiedName :: Port -> String
portQualifiedName Port{portClient = Client{clientName}, portName} = concat [
      clientName
    , "."
    , portName
    ]

{-------------------------------------------------------------------------------
  List all
-------------------------------------------------------------------------------}

getAllPorts :: ALSA.Handle -> IO [Port]
getAllPorts h = fmap (filter (not . isUs) . concat) <$>
    Client.Info.queryLoop (Handle.alsa h) $ \info -> do
      client <- getClient info
      getClientPorts h client
  where
    -- Avoid listing ourselves amongst the available clients
    isUs :: Port -> Bool
    isUs p = clientId (portClient p) == Handle.client h

getClientPorts :: ALSA.Handle -> Client -> IO [Port]
getClientPorts h client@Client{clientId} =
    Port.Info.queryLoop (Handle.alsa h) clientId $ \info ->
      getPort client info

getClient :: Client.Info.T -> IO Client
getClient info = do
    clientId   <- Client.Info.getClient info
    clientName <- Client.Info.getName   info
    return Client{
        clientId
      , clientName
      }

getPort :: Client -> Port.Info.T -> IO Port
getPort portClient info = do
    portId   <- Port.Info.getPort info
    portName <- Port.Info.getName info

    let portAddress :: Address.T
        portAddress = Address.Cons {
            client = clientId portClient
          , port   = portId
          }

    return Port{
        portClient
      , portId
      , portName
      , portAddress
      }

{-------------------------------------------------------------------------------
  Find specified port
-------------------------------------------------------------------------------}

data FindPortResult =
    PortNotFound
  | PortAmbiguous [Port]
  | PortFound Port

findPort :: ALSA.Handle -> String -> IO FindPortResult
findPort h name =
    mkResult . filter isMatch <$> getAllPorts h
  where
    isMatch :: Port -> Bool
    isMatch port = name `isInfixOf` portQualifiedName port

    mkResult :: [Port] -> FindPortResult
    mkResult []  = PortNotFound
    mkResult [p] = PortFound p
    mkResult ps  = PortAmbiguous ps