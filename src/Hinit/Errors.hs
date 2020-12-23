{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}

module Hinit.Errors where

import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Terminal
import Control.Exception
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Void
import GHC.Generics
import Hinit.Utils
import Path
import System.Exit
import System.IO
import Text.Megaparsec.Error
import Text.Mustache.Render
import qualified Text.Parsec.Error as T
import Toml.Codec.Error
import Prelude hiding (print)

data ExprParseError = ExprParseError
  { expression :: Text,
    errors :: [ParseError Text Void]
  }
  deriving (Eq, Generic)
  deriving (Show) via PrettyShow ExprParseError
  deriving anyclass (Exception)

instance Pretty ExprParseError where
  pretty ExprParseError {..} =
    vsep
      [ [i|failed to parse boolean expressionession "#{expression}":|],
        indent 2 $ mkBulletList $ map (pretty . parseErrorPretty) errors
      ]

data TomlFile
  = Global
  | Template Text
  deriving (Eq)

instance Show TomlFile where
  show Global = "global config file (~/.config/hi/config.toml)"
  show (Template t) = [i|template config file from #{t}|]

data ConfigParseError = ConfigParseError
  { file :: TomlFile,
    errors :: [TomlDecodeError]
  }
  deriving (Eq, Generic)
  deriving (Show) via PrettyShow ConfigParseError
  deriving anyclass (Exception)

instance Pretty ConfigParseError where
  pretty ConfigParseError {..} =
    vsep
      [ [i|failed to parse #{file}:|],
        indent 2 $ mkBulletList $ map (pretty . prettyTomlDecodeError) errors
      ]

data MustacheError
  = forall a. RenderingError (Path Rel a) Bool [SubstitutionError]
  | forall a. TemplateParseError (Path Rel a) Bool T.ParseError
  deriving (Show) via PrettyShow MustacheError
  deriving anyclass (Exception)

instance Pretty MustacheError where
  pretty e
    | (RenderingError p isFilename errors) <- e =
      vsep
        [ [i|failed to substitute the #{s isFilename} of file #{toFilePath p}:|],
          indent 2 $ mkBulletList $ map viaShow errors
        ]
    | (TemplateParseError p isFilename err) <- e =
      vsep
        [ [i|failed to parse the #{s isFilename} of file #{toFilePath p}:|],
          indent 2 $ viaShow err
        ]
    where
      s :: Bool -> String
      s True = "filename"
      s False = "contents"

data IllformedPath
  = TemplateName Text
  | ProjectName Text
  | forall a. TemplateFile (Path Rel a) Text
  deriving (Show) via PrettyShow IllformedPath
  deriving anyclass (Exception)

instance Pretty IllformedPath where
  pretty (TemplateName s) =
    [i|illformed template name "#{s}" (there should be no slashes in template names)|]
  pretty (ProjectName s) =
    [i|illformed project name "#{s}" (there should be no slashes in project names)|]
  pretty (TemplateFile p f) =
    [i|file "#{toFilePath p}"" has illformed name "#{f}" after substitution|]

newtype TemplateNotFound
  = TemplateNotFound Text
  deriving (Eq, Generic)
  deriving (Show) via PrettyShow TemplateNotFound
  deriving anyclass (Exception)

instance Pretty TemplateNotFound where
  pretty (TemplateNotFound t) =
    [i|template #{t} not found in both bundled templates and local templates|]

newtype ProjectAlreadExist
  = ProjectAlreadExist Text
  deriving (Eq, Generic)
  deriving (Show) via PrettyShow ProjectAlreadExist
  deriving anyclass (Exception)

instance Pretty ProjectAlreadExist where
  pretty (ProjectAlreadExist a) =
    [i|project #{a} already exists, to overwrite it use -f/--force|]

prettyPrintError :: Has Terminal sig m => Doc AnsiStyle -> m ()
prettyPrintError err = prettyPrint stderr doc
  where
    doc = (annotate (color Red) "Error" <> ":") <+> err

simpleHandler :: (Has Terminal sig m, Has (Lift IO) sig m, Pretty a) => a -> m ()
simpleHandler a = do
  prettyPrintError $ pretty a
  sendIO exitFailure
