module Listen.Cmdline (
    Cmdline(..)
  , Mode(..)
  , getCmdline
  ) where

import Options.Applicative

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdMode :: Mode
    }
  deriving stock (Show)

data Mode =
    ModeListPorts
  | ModeListen String
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
    , command' "listen" (ModeListen <$> parsePort)
        "Listen"
    ]

parsePort :: Parser String
parsePort = strOption $ mconcat [
      short 'p'
    , long "port"
    , help "Port"
    , metavar "NAME"
    ]

{-------------------------------------------------------------------------------
  Auxiliary optparse-applicative
-------------------------------------------------------------------------------}

command' :: String -> Parser a -> String -> Mod CommandFields a
command' name p desc = command name $ info p $ mconcat [
      progDesc desc
    ]
