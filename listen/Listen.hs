module Listen (main) where

import Control.Monad
import Data.List (intercalate)

import EurekaPROM.IO           qualified as IO
import EurekaPROM.IO.Discovery qualified as Discovery
import EurekaPROM.IO.Alsa      qualified as Alsa

import Listen.Cmdline

main :: IO ()
main = Alsa.init $ \alsa -> do
    cmdline <- getCmdline
    case cmdMode cmdline of
      ModeListPorts -> do
        clients <- Discovery.getAllPorts alsa
        forM_ clients $ \(client, ports) ->
          forM_ ports $ \port ->
              putStrLn $ Discovery.clientPortName client port
      ModeListen portName -> do
        mPort <- Discovery.findPort alsa portName
        case mPort of
          Discovery.PortNotFound ->
            fail "Port not found"
          Discovery.PortAmbiguous ports ->
            fail $ concat [
                "Port ambiguous: "
              , intercalate ", " $
                  map (uncurry Discovery.clientPortName) ports
              ]
          Discovery.PortFound client port ->
            putStrLn $ "Using " ++ Discovery.clientPortName client port