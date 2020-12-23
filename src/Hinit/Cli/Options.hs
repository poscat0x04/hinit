{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Hinit.Cli.Options where

import Control.Effect.Lift
import Data.String.Interpolate
import Data.Text (Text)
import Data.Version
import GHC.Generics
import Hinit.Types
import Options.Applicative
import Paths_hinit

data Op
  = Set Text Val
  deriving (Show, Eq, Generic)

toPair :: Op -> (Text, Val)
toPair (Set t v) = (t, v)

data Command
  = Init
      { template :: Text,
        project :: Text,
        ops :: [Op],
        force :: Bool
      }
  | List
      { verbose :: Bool
      }
  deriving (Show, Generic)

verString :: String
verString = [i|hi version #{showVersion version}|]

readBool :: ReadM Bool
readBool = do
  s <- str
  if
      | s == "True" || s == "true" -> pure True
      | s == "False" || s == "false" -> pure False
      | otherwise -> fail ("the option " <> s <> " is not a bool")

text :: Parser (Text, Val)
text = (,) <$> arg <*> val
  where
    arg =
      strOption
        ( long "text"
            <> short 't'
            <> metavar "Key"
            <> help "Set a key-value pair where the value is text"
        )
    val = Text <$> argument str (metavar "Value")

bool :: Parser (Text, Val)
bool = (,) <$> arg <*> val
  where
    arg =
      strOption
        ( long "bool"
            <> short 'b'
            <> metavar "Key"
            <> help "Set a key-value pair where the value is boolean"
        )
    val =
      Bool
        <$> argument
          readBool
          ( metavar "Value"
              <> completeWith ["True", "False"]
          )

operation :: Parser Op
operation = setT <|> setB
  where
    setT =
      uncurry Set <$> text
    setB =
      uncurry Set <$> bool

initOptions :: Parser Command
initOptions =
  Init
    <$> argument str (metavar "TEMPLATE")
    <*> argument str (metavar "TARGET")
    <*> many operation
    <*> switch (long "force" <> short 'f')

listOptions :: Parser Command
listOptions =
  List
    <$> switch (long "verbose" <> short 'v')

commandParser :: Parser Command
commandParser =
  hsubparser
    ( foldMap
        (\(cmd, parser, desc) -> command cmd (info (parser <**> versionHelper) (progDesc desc)))
        [ ("init", initOptions, "Initialze a project"),
          ("list", listOptions, "List all available templates")
        ]
    )

versionHelper :: Parser (a -> a)
versionHelper = infoOption verString (long "version" <> help "print program version")

cmds :: ParserInfo Command
cmds =
  info
    (commandParser <**> helper <**> versionHelper)
    ( fullDesc
        <> progDesc
          "hi is a generic project scaffolding tool that uses mustache for templating.\n\
          \For more documentation, see https://github.com/poscat0x04/hinit"
        <> header "hi - Project scaffolding tool writting in Haskell"
    )

parseCliOptions :: Has (Lift IO) sig m => m Command
parseCliOptions = sendIO $ execParser cmds
