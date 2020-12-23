{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Global configuration
module Hinit.Config where

import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Terminal
import Control.Effect.Throw
import Control.Effect.Time as T
import qualified Data.Map.Strict as M
import Data.String.Interpolate
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import Data.Time.Calendar
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Distribution.Parsec
import Distribution.Pretty
import Distribution.SPDX.Extra
import GHC.Generics
import Hinit.Errors
import Hinit.Types
import Path
import Path.IO
import System.IO
import Toml hiding (Bool, Text, day)
import qualified Toml as T

data Config = Config
  { name :: Text,
    email :: Text,
    ghUserName :: Text,
    license :: Maybe LicenseId,
    vcs :: Maybe VCS,
    defAttrs :: Context
  }
  deriving (Show, Eq, Generic)

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> text "name" .= name
    <*> text "email" .= email
    <*> text "github_username" .= ghUserName
    <*> dioptional (licenseIdCodec "license") .= license
    <*> vcsCodec "vcs" .= vcs
    <*> contextCodec .= defAttrs

licenseToToml :: LicenseId -> AnyValue
licenseToToml license = AnyValue $ T.Text $ pack $ prettyShow license

tomlToLicense :: AnyValue -> Either TomlBiMapError LicenseId
tomlToLicense (AnyValue v)
  | T.Text t <- v =
    case eitherParsec (unpack t) of
      Left err ->
        Left $
          ArbitraryError [i|Failed to parse license: #{err}|]
      Right l -> pure l
  | otherwise = Left $ WrongValue $ MatchError TText (AnyValue v)

_LicenseId :: TomlBiMap LicenseId AnyValue
_LicenseId = invert $ prism licenseToToml tomlToLicense

licenseIdCodec :: Key -> TomlCodec LicenseId
licenseIdCodec = match _LicenseId

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
  let license = Nothing
  pure Config {..}
  where
    askText :: m Text
    askText = do
      resp <- prompt "> "
      maybe askText pure resp

getConfigFile :: (Has (Lift IO) sig m) => m (Path Abs File)
getConfigFile = do
  configDir <- sendIO $ getXdgDir XdgConfig $ Just [reldir|hi|]
  sendIO $ ensureDir configDir
  let configFile = configDir </> [relfile|config.toml|]
  pure configFile

readConfig :: (Has (Lift IO) sig m, Has (Throw ConfigParseError) sig m) => Path Abs File -> m (Maybe Config)
readConfig configFile = do
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
getConfig = do
  configFile <- getConfigFile
  mConfig <- readConfig configFile
  case mConfig of
    Just c -> pure c
    Nothing -> do
      config <- askConfig
      sendIO $ T.writeFile (fromAbsFile configFile) $ encode configCodec config
      pure config

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
