module App.Common.Cmdline (
    -- * PortSpec
    parsePortSpec
  , parsePortDual
  , parsePortInputOnly
  , parsePortOutputOnly
  , parsePortDuplex
  ) where

import Options.Applicative

import Control.ALSA qualified as ALSA

{-------------------------------------------------------------------------------
  PortSpec
-------------------------------------------------------------------------------}

parsePortDual :: Parser ALSA.PortSpec
parsePortDual =
    pure ALSA.PortDual
      <*> (strOption $ mconcat [
              long "input-port"
            , help "Input port"
            ])
      <*> (strOption $ mconcat [
              long "output-port"
            , help "Output port"
            ])

parsePortInputOnly :: Parser ALSA.PortSpec
parsePortInputOnly =
    pure ALSA.PortInputOnly
      <*> (strOption $ mconcat [
              long "input-only"
            , help "Input port"
            ])

parsePortOutputOnly :: Parser ALSA.PortSpec
parsePortOutputOnly =
    pure ALSA.PortOutputOnly
      <*> (strOption $ mconcat [
              long "output-only"
            , help "Output port"
            ])

parsePortDuplex :: Parser ALSA.PortSpec
parsePortDuplex =
     pure ALSA.PortDuplex
        <*> (strOption $ mconcat [
                 long "duplex-port"
               , help "Port name (for both input and output)"
               ])

parsePortSpec :: Parser ALSA.PortSpec
parsePortSpec = asum [
      parsePortDual
    , parsePortInputOnly
    , parsePortOutputOnly
    , parsePortDuplex
    ]
