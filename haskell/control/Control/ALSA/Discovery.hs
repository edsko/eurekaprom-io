-- | Utilities for finding the MIDI device
--
-- Intended for qualified import.
--
-- > import Control.ALSA.Discovery qualified as Discovery
module Control.ALSA.Discovery (
    -- * Definition
    Client(..)
  , ClientName
  , Port(..)
  , PortName
  , portQualifiedName
    -- * Discovery
  , getAllPorts
  , FindPortException(..)
  , findPort
  ) where

import Control.Exception
import Data.List (isInfixOf)

import "alsa-seq" Sound.ALSA.Sequencer.Address     qualified as Address
import "alsa-seq" Sound.ALSA.Sequencer.Client      qualified as Client
import "alsa-seq" Sound.ALSA.Sequencer.Client.Info qualified as Client.Info
import "alsa-seq" Sound.ALSA.Sequencer.Port        qualified as Port
import "alsa-seq" Sound.ALSA.Sequencer.Port.Info   qualified as Port.Info

import Control.ALSA.Handle qualified as ALSA (Handle)
import Control.ALSA.Handle qualified as Handle

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Client = Client {
      clientId   :: Client.T
    , clientName :: ClientName
    }
  deriving (Show)

data Port = Port {
      portClient  :: Client
    , portId      :: Port.T
    , portName    :: PortName
    , portAddress :: Address.T
    }

type ClientName = String
type PortName   = String

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

data FindPortException =
    PortNotFound PortName
  | PortAmbiguous PortName [PortName]
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Find port by name
--
-- May throw 'FindPortException'.
findPort :: ALSA.Handle -> PortName -> IO Port
findPort h name =
    mkResult . filter isMatch =<< getAllPorts h
  where
    isMatch :: Port -> Bool
    isMatch port = name `isInfixOf` portQualifiedName port

    mkResult :: [Port] -> IO Port
    mkResult [p] = return p
    mkResult []  = throwIO $ PortNotFound name
    mkResult ps  = throwIO $ PortAmbiguous name (map portName ps)