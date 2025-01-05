-- | Utilities for writing applications using @eurekaprom-io@.
module EurekaPROM.IO.App (
    -- * Discovery
    listPorts
  , findPort
  ) where

import Control.Monad
import Data.List (intercalate)
import System.Exit
import System.IO

import EurekaPROM.IO.ALSA qualified as ALSA

{-------------------------------------------------------------------------------
  Discovery
-------------------------------------------------------------------------------}

listPorts :: ALSA.Handle -> IO ()
listPorts h = do
    ports <- ALSA.getAllPorts h
    forM_ ports $ \port ->
      putStrLn $ ALSA.portQualifiedName port

findPort :: ALSA.Handle -> String -> (ALSA.Port -> IO r) -> IO r
findPort h portName k = do
    mPort <- ALSA.findPort h portName
    case mPort of
      ALSA.PortNotFound -> do
        hPutStrLn stderr "Port not found"
        exitFailure
      ALSA.PortAmbiguous ports -> do
        hPutStrLn stderr $ concat [
            "Port ambiguous: "
          , intercalate ", " $ map ALSA.portQualifiedName ports
          ]
        exitFailure
      ALSA.PortFound port -> do
        putStrLn $ "Using " ++ ALSA.portQualifiedName port
        k port
