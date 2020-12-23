module Hinit.Optionals where

import Control.Effect.Throw
import Data.Algebra.Boolean
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import Hinit.Errors
import Hinit.Template.Config
import Hinit.Types
import System.FilePath.Glob
import Text.Megaparsec

defaultIgnores :: [Pattern]
defaultIgnores =
  [ "template.toml",
    ".git",
    ".pijul",
    ".hg",
    "_darcs",
    ".gitkeep",
    "**/.gitkeep"
  ]

parseExpr :: Has (Throw ExprParseError) sig m => Text -> m Expr
parseExpr expr' =
  case runParser expr "" expr' of
    Left ParseErrorBundle {..} ->
      throwError $ ExprParseError expr' $ NE.toList bundleErrors
    Right e -> pure e

booleanContext :: Context -> CTX
booleanContext ctx = M.fromList $ mapMaybe m $ M.toList ctx
  where
    m (t, Bool b) = Just (t, b)
    m _ = Nothing

ignoredFiles ::
  Has (Throw ExprParseError) sig m =>
  Context ->
  TemplateConfig ->
  m [Pattern]
ignoredFiles ctx TemplateConfig {..} = do
  l <- traverse ignoredFiles' optionals
  pure $ defaultIgnores <> ignores <> concat l
  where
    ctx' = booleanContext ctx
    ignoredFiles' Optional {..} = do
      expr' <- parseExpr when
      if eval ctx' expr'
        then pure ignores
        else pure []
