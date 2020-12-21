{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Global configuration
module HI.Config where

import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Terminal
import Control.Effect.Throw
import Control.Effect.Time as T
import qualified Data.Map.Strict as M
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Data.Time.Calendar
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import GHC.Generics
import HI.Errors
import HI.Types
import Path
import Path.IO
import System.IO
import Toml hiding (Bool, Text, day)

data Config = Config
  { name :: Text,
    email :: Text,
    ghUserName :: Text,
    vcs :: Maybe VCS,
    defAttrs :: Context
  }
  deriving (Show, Eq, Generic)

askConfig :: forall sig m. Has Terminal sig m => m Config
askConfig = do
  prettyPrint stdout "No config has been found, creating a new config (~/.config/hi/config.toml)"
  prettyPrint stdout "Please enter your name:"
  name <- askText
  prettyPrint stdout "Your email:"
  email <- askText
  prettyPrint stdout "Your github username:"
  ghUserName <- askText
  let vcs = Just Git
  let defAttrs = mempty
  pure Config {..}
  where
    askText :: m Text
    askText = do
      resp <- prompt "> "
      maybe askText pure resp

readConfig :: (Has (Lift IO) sig m, Has (Throw ConfigParseError) sig m) => m (Maybe Config)
readConfig = do
  configDir <- sendIO $ getXdgDir XdgConfig $ Just [reldir|hi|]
  sendIO $ ensureDir configDir
  let configFile = configDir </> [relfile|config.toml|]
  exists <- sendIO $ doesFileExist configFile
  if exists
    then do
      f <- sendIO $ T.readFile $ fromAbsFile configFile
      case decode configCodec f of
        Left e -> throwError $ ConfigParseError Global e
        Right config -> pure (Just config)
    else pure Nothing

getConfig ::
  ( Has Terminal sig m,
    Has (Lift IO) sig m,
    Has (Throw ConfigParseError) sig m
  ) =>
  m Config
getConfig = readConfig >>= maybe askConfig pure

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> text "name" .= name
    <*> text "email" .= email
    <*> text "github_username" .= ghUserName
    <*> vcsCodec "vcs" .= vcs
    <*> contextCodec .= defAttrs

-- | Build two 'Context's from a program config
buildContextFromConfig :: Has Time sig m => Text -> Config -> m (Context, Context)
buildContextFromConfig project Config {..} = do
  ZonedTime {..} <- T.getZonedTime
  let lday = localDay zonedTimeToLocalTime
  let (show' -> year, show' -> month, show' -> day) = toGregorian lday
  let iso8601 = pack $ iso8601Show lday
  let overrides =
        [ ("year", Text year),
          ("month", Text month),
          ("day", Text day),
          ("iso8601", Text iso8601),
          ("name", Text name),
          ("email", Text email),
          ("github_username", Text ghUserName),
          ("project", Text project)
        ]
  pure (M.fromList overrides, defAttrs)
  where
    show' :: Show a => a -> Text
    show' = pack . show
