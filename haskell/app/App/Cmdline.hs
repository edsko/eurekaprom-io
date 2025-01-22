module App.Cmdline (
    Cmdline(..)
  , Mode(..)
  , getCmdline
  ) where

import Options.Applicative

import EurekaPROM.IO.ALSA qualified as ALSA

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdMode :: Mode
    }
  deriving stock (Show)

data Mode =
    ModeListPorts
  | ModeDump ALSA.PortSpec
  | ModeSimEvents ALSA.PortSpec
  | ModeTestLEDs ALSA.PortSpec
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = execParser opts
  where
    opts :: ParserInfo Cmdline
    opts = info (parseCmdline <**> helper) $ mconcat [
        fullDesc
      , progDesc "Listen to output from the EurekaPROM"
      ]

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseCmdline :: Parser Cmdline
parseCmdline =
    pure Cmdline
      <*> parseMode

parseMode :: Parser Mode
parseMode = subparser $ mconcat [
      command' "list-ports" (pure ModeListPorts)
        "List available ports"
    , command' "dump" (ModeDump <$> parsePortSpec)
        "Dump all output from the FCB1010"
    , command' "simulate" (ModeSimEvents <$> parsePortSpec)
        "Simulate keyboard and mouse events"
    , command' "test-LEDs" (ModeTestLEDs <$> parsePortSpec)
        "Test the LEDs"
    ]

parsePortSpec :: Parser ALSA.PortSpec
parsePortSpec = asum [
      pure ALSA.DualPort
        <*> (strOption $ mconcat [
                long "input-port"
              , help "Input port"
              ])
        <*> (strOption $ mconcat [
                long "output-port"
              , help "Output port"
              ])
    , pure ALSA.SinglePort
        <*> (strOption $ mconcat [
                 long "port"
               , help "Port name (for both input and output)"
               ])
    ]

{-------------------------------------------------------------------------------
  Auxiliary optparse-applicative
-------------------------------------------------------------------------------}

command' :: String -> Parser a -> String -> Mod CommandFields a
command' name p desc = command name $ info p $ mconcat [
      progDesc desc
    ]
