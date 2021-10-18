{-# LANGUAGE QuasiQuotes #-}

module Hinit.Template where

import Control.Effect.Lift
import Control.Effect.Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.String.Interpolate
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import GHC.Generics
import Hinit.Errors
import Hinit.Template.Config
import Hinit.Types
import Hinit.Utils
import Path
import Path.IO
import Paths_hinit
import Prettyprinter
import System.FilePath.Glob
import Text.Mustache hiding (Template)
import Text.Mustache.Types (Value)
import Toml (decode)
import Toml.Codec.Error (TomlDecodeError)

data Template
  = Local
      { path :: Path Abs Dir,
        name :: Text,
        templateConfig :: TemplateConfig
      }
  | Broken
      { path :: Path Abs Dir,
        name :: Text,
        errors :: [TomlDecodeError]
      }
  deriving (Show, Eq, Generic)

fromContext :: Context -> Value
fromContext = object . fmap (uncurry (~>)) . M.toList

underCurrentDir :: Path Rel a -> Bool
underCurrentDir = (== [reldir|./|]) . parent

-- | Copy a file from template to the corresponding location in the target directory and apply substitution.
--   Note that this function assumes that the parent directory of the target file exists.
copyFileFromTemplate ::
  ( Has (Throw MustacheError) sig m,
    Has (Throw IllformedPath) sig m,
    Has (Lift IO) sig m,
    ToMustache ctx
  ) =>
  -- | Mustache context
  ctx ->
  -- | Base dir of the template
  Path a Dir ->
  -- | Base dir of the target
  Path b Dir ->
  -- | Source file path relative to the template base
  Path Rel File ->
  m ()
copyFileFromTemplate ctx base tgtbase src = do
  let srcFilename = filename src
  let srcParentDir = parent src
  case compileTemplate "" (pack $ fromRelFile srcFilename) of
    Left e -> throwError $ TemplateParseError src True (pack $ show e)
    Right fileNameTmpl -> do
      let (errs, tgtFileNameRaw) = checkedSubstitute fileNameTmpl ctx
      unless (null errs) $
        throwError $ RenderingError src True errs
      let err = TemplateFile src tgtFileNameRaw
      case parseRelFile (unpack tgtFileNameRaw) of
        Nothing -> throwError err
        Just tgtFileName
          | not (underCurrentDir tgtFileName) -> throwError err
          | otherwise -> do
            let srcFilePath = base </> src
            let tgtFilePath = tgtbase </> srcParentDir </> tgtFileName
            srcFileContent <- sendIO $ T.readFile (toFilePath srcFilePath)
            case compileTemplate (toFilePath src) srcFileContent of
              Left e' -> throwError $ TemplateParseError src False (pack $ show e')
              Right fileTmpl -> do
                let (errs', tgtFileContent) = checkedSubstitute fileTmpl ctx
                unless (null errs') $
                  throwError $ RenderingError src False errs'
                sendIO $ T.writeFile (toFilePath tgtFilePath) tgtFileContent

-- | Copy a directory from template to the corresponding location in the target directory and apply substitution.
--   Note that this function assumes that the parent directory of the target directory exists.
copyDirFromTemplate ::
  ( Has (Throw MustacheError) sig m,
    Has (Throw IllformedPath) sig m,
    Has (Lift IO) sig m,
    ToMustache ctx
  ) =>
  -- | Mustache context
  ctx ->
  -- | Base dir of the template
  Path a Dir ->
  -- | Base dir of the target
  Path b Dir ->
  -- | Source dir path relative to the template base
  Path Rel Dir ->
  m ()
copyDirFromTemplate ctx _ tgtbase src = do
  let srcDirName = dirname src
  let srcParentDir = parent src
  case compileTemplate "" (pack $ fromRelDir srcDirName) of
    Left e -> throwError $ TemplateParseError src True (pack $ show e)
    Right fileNameTmpl -> do
      let (errs, tgtFileNameRaw) = checkedSubstitute fileNameTmpl ctx
      unless (null errs) $
        throwError $ RenderingError src True errs
      let err = TemplateFile src tgtFileNameRaw
      case parseRelDir (unpack tgtFileNameRaw) of
        Nothing -> throwError err
        Just tgtDirName
          | not (underCurrentDir tgtDirName) -> throwError err
          | otherwise -> do
            let tgtDirPath = tgtbase </> srcParentDir </> tgtDirName
            sendIO $ createDirIfMissing False tgtDirPath

-- | Initialize a project from a template
initFromTemplate ::
  ( Has (Throw MustacheError) sig m,
    Has (Throw IllformedPath) sig m,
    Has (Lift IO) sig m,
    MonadIO m,
    ToMustache ctx
  ) =>
  -- | Ignored files
  [Pattern] ->
  -- | Mustache context
  ctx ->
  -- | Base dir of the template
  Path a Dir ->
  -- | Base dir of the target
  Path b Dir ->
  m ()
initFromTemplate ignores ctx template target = do
  sendIO $ ensureDir target
  walkDirRel handler template
  where
    handler current subdirs files = do
      traverse_ (copyFileFromTemplate ctx template target) filesToCpy
      traverse_ (copyDirFromTemplate ctx template target) dirsToCpy
      pure $ WalkExclude excl
      where
        subdirs' = map (current </>) subdirs
        files' = map (current </>) files
        ignored :: Path Rel a -> Bool
        ignored a = ignores `matches` toFilePath a
        excl = filter ignored subdirs'
        dirsToCpy = filter (not . ignored) subdirs'
        filesToCpy = filter (not . ignored) files'

readTemplate :: Has (Lift IO) sig m => Path Abs Dir -> m (Maybe Template)
readTemplate templatePath = do
  let templateConfigFile = templatePath </> [relfile|template.toml|]
  let name = pack $ init $ fromRelDir $ dirname templatePath
  exists <- sendIO $ doesFileExist templateConfigFile
  if exists
    then do
      f <- sendIO $ T.readFile $ fromAbsFile templateConfigFile
      case decode templateConfigCodec f of
        Left errors -> pure $ Just Broken {path = templatePath, ..}
        Right templateConfig -> pure $ Just Local {path = templatePath, ..}
    else pure Nothing

getTemplateDirs :: Has (Lift IO) sig m => m [Path Abs Dir]
getTemplateDirs = do
  dataDirRaw <- sendIO getDataDir
  dataDir <- sendIO $ parseAbsDir dataDirRaw
  let bundledTemplateDir = dataDir </> [reldir|templates|]
  localTemplateDir <- sendIO $ getXdgDir XdgData (Just [reldir|hi|])
  let dirs = [bundledTemplateDir, localTemplateDir]
  traverse_ (sendIO . ensureDir) dirs
  pure dirs

getTemplate ::
  ( Has (Lift IO) sig m,
    Has (Throw IllformedPath) sig m
  ) =>
  Text ->
  m (Maybe Template)
getTemplate tmpl = do
  templateDirs <- getTemplateDirs
  case parseRelDir (unpack tmpl) of
    Nothing -> throwError $ TemplateName tmpl
    Just templateName -> do
      let templates = fmap (</> templateName) templateDirs
      mTemplates <- traverse readTemplate templates
      pure $ getFirst $ foldMap First mTemplates

getTemplates :: Has (Lift IO) sig m => m [Template]
getTemplates = do
  templateDirs <- getTemplateDirs
  contents <- traverse (sendIO . listDir) templateDirs
  let subdirs = concatMap fst contents
  mTemplates <- traverse readTemplate subdirs
  pure $ catMaybes mTemplates

prettyTemplate :: Bool -> Template -> Doc a
prettyTemplate verbose template
  | Broken {..} <- template =
    if verbose
      then
        vsep
          [ [i|template: #{name}|],
            [i|path: #{path}|]
          ]
      else pretty name
  | Local {..} <- template,
    TemplateConfig {..} <- templateConfig =
    if verbose
      then
        vsep $
          [i|template: #{name}|] :
          maybe [] (\d -> pure [i|desc: #{d}|]) desc
            ++ vsep
              [ "tags:",
                indent 2 $ mkBulletList $ fmap pretty tags
              ] :
          pure [i|path: #{path}|]
      else pretty name

prettyTemplates :: Bool -> [Template] -> Doc a
prettyTemplates verbose templates =
  vsep $
    [ "- local templates",
      indent 2 $ mkBulletList local
    ]
      ++ if null broken
        then []
        else
          [ "- broken templates",
            indent 2 $ mkBulletList broken
          ]
  where
    isBroken Broken {} = True
    isBroken _ = False
    isLocal Local {} = True
    isLocal _ = False
    prettyP = prettyTemplate verbose
    broken = map prettyP $ filter isBroken templates
    local = map prettyP $ filter isLocal templates
