{-# LANGUAGE QuasiQuotes #-}

module Hinit.License where

import Control.Effect.Lift
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text.IO as T
import Distribution.Pretty
import Distribution.SPDX (LicenseId)
import Distribution.SPDX.Template
import Hinit.Types
import Path
import Paths_hinit
import Text.Megaparsec

lookupT :: Text -> Context -> Maybe Text
lookupT k ctx = do
  v <- M.lookup k ctx
  case v of
    (Text t) -> pure t
    _ -> Nothing

getLicenseFile :: Has (Lift IO) sig m => LicenseId -> m License
getLicenseFile licenseId = do
  dataDir <- sendIO getDataDir
  let licenseFileName = prettyShow licenseId <> ".template.txt"
  let licenseFilePath = dataDir <> "/licenses/" <> licenseFileName
  licenseFile <- sendIO $ T.readFile licenseFilePath
  case runParser license licenseFileName licenseFile of
    Right l -> pure l
    Left e -> error $ "impossible, failed to parse license file:\n" <> errorBundlePretty e

buildSPDXContext :: Context -> Map Text Text
buildSPDXContext ctx = fromMaybe mempty mCtx
  where
    mCtx = do
      name <- lookupT "name" ctx
      year <- lookupT "year" ctx
      let copyright = [i|Copyright (c) #{year} #{name}|]
      pure $ M.singleton "copyright" copyright

initializeLicense ::
  (Has (Lift IO) sig m) => LicenseId -> Context -> Path a Dir -> m ()
initializeLicense licenseId ctx projectPath = do
  let targetFile = projectPath </> [relfile|LICENSE|]
  licenseFile <- getLicenseFile licenseId
  let spdxCtx = buildSPDXContext ctx
  let mRendered = render spdxCtx licenseFile
  case mRendered of
    Right rendered -> sendIO $ T.writeFile (toFilePath targetFile) rendered
    Left e -> error [i|impossible, failed to render license: #{e}|]
