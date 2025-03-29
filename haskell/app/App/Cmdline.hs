module App.Cmdline (
    Cmdline(..)
  , Mode(..)
  , getCmdline
  ) where

import Options.Applicative

import Control.ALSA qualified as ALSA

import App.Common.Cmdline
import App.Mode.GenMealy qualified as Mode.GenMealy

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
  | ModeGenMealy (Mode.GenMealy.Cmd ALSA.PortSpec)
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
    , command' "generate-mealy" (ModeGenMealy <$> parseGenMeadyCmd)
        "Generate Mealy machine."
    ]

parseGenMeadyCmd :: Parser (Mode.GenMealy.Cmd ALSA.PortSpec)
parseGenMeadyCmd = asum [
      Mode.GenMealy.Exec <$> parsePortSpec
    , Mode.GenMealy.Yaml <$> strOption (mconcat [
          long "yaml"
        , help "Export to human-readable .yaml format"
        ])
    , Mode.GenMealy.Json <$> strOption (mconcat [
          long "json"
        , help "Export to machine-readable .json format"
        ])
    ]

{-------------------------------------------------------------------------------
  Auxiliary optparse-applicative
-------------------------------------------------------------------------------}

command' :: String -> Parser a -> String -> Mod CommandFields a
command' name p desc = command name $ info (p <**> helper) $ mconcat [
      progDesc desc
    ]
