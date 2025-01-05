module Listen (main) where

import Control.Monad
import Data.List (intercalate)

import EurekaPROM.IO qualified as IO

import Listen.Cmdline

main :: IO ()
main = IO.init $ \h -> do
    cmdline <- getCmdline
    case cmdMode cmdline of
      ModeListPorts -> do
        clients <- IO.getAllPorts h
        forM_ clients $ \(client, ports) ->
          forM_ ports $ \port ->
              putStrLn $ IO.clientPortName client port
      ModeListen portName -> do
        mPort <- IO.findPort h portName
        case mPort of
          IO.PortNotFound ->
            fail "Port not found"
          IO.PortAmbiguous ports ->
            fail $ concat [
                "Port ambiguous: "
              , intercalate ", " $
                  map (uncurry IO.clientPortName) ports
              ]
          IO.PortFound client port addr -> do
            putStrLn $ "Using " ++ IO.clientPortName client port
            IO.listen h addr
