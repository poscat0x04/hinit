{-# LANGUAGE OverloadedLabels #-}

module Hinit.Context where

import Control.Algebra
import Control.Effect.Terminal
import Control.Effect.Time
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Hinit.Cli
import Hinit.Cli.Options
import Hinit.Config
import Hinit.Template.Config
import Hinit.Types

opsToCtx :: [Op] -> Context
opsToCtx = M.fromList . map toPair

buildContextFromOptions :: forall m sig. Has Terminal sig m => [Option] -> m Context
buildContextFromOptions = fmap M.fromList . traverse buildContext'
  where
    buildContext' :: Option -> m (Text, Val)
    buildContext' BoolOpt {..} = do
      b <- case defB of
        Just b -> pure $ Bool b
        Nothing -> query Bool' name desc
      pure (name, b)
    buildContext' TextOpt {..} = do
      t <- case defT of
        Just t -> pure $ Text t
        Nothing -> query Text' name desc
      pure (name, t)

buildContext ::
  (Has Time sig m, Has Terminal sig m) =>
  Config ->
  TemplateConfig ->
  [Op] ->
  Text ->
  m Context
buildContext cfg TemplateConfig {..} ops project = do
  (topPrio, custom) <- buildContextFromConfig project cfg
  tmplCtx <- buildContextFromOptions options
  let overrides = opsToCtx ops
  pure (topPrio <> overrides <> tmplCtx <> custom)
