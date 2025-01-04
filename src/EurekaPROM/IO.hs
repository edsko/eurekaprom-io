module EurekaPROM.IO (go) where

import Sound.ALSA.Sequencer             qualified as ALSA
import Sound.ALSA.Sequencer.Client      qualified as Client
import Sound.ALSA.Sequencer.Port        qualified as Port
import Sound.ALSA.Sequencer.Queue       qualified as Queue
import Sound.ALSA.Sequencer.Query       qualified as Query
import Sound.ALSA.Sequencer.Client.Info qualified as Client.Info

-- This is just a test for now
go :: IO ()
go =
    ALSA.withDefault @ALSA.DuplexMode ALSA.Nonblock $ \alsa  ->
      Client.Info.queryLoop_ alsa $ \client -> do
        name <- Client.Info.getName client
        putStrLn $ name




{-
    ALSA.withDefault @ALSA.DuplexMode ALSA.Nonblock    $ \alsa  ->
    Port.withSimple alsa "main port" portCaps portType $ \port  ->
    Queue.withNamed alsa "main queue"                  $ \queue ->
    do
      Client.setName alsa "eurekaprom-io"
      clientId <- Client.getId alsa
      undefined
  where
    portCaps :: Port.Cap
    portCaps = Port.caps [Port.capWrite, Port.capSubsWrite]

    portType :: Port.Type
    portType = Port.typeMidiGeneric
-}
