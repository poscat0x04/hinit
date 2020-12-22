{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Carrier.Error.Church
import Control.Carrier.Lift
import Control.Effect.Terminal
import Control.Effect.Time
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import HI.Cli
import HI.Cli.Options
import HI.Config
import HI.Context
import HI.Errors
import HI.License
import HI.Optionals
import HI.Template
import Path.IO
import System.IO

main :: IO ()
main =
  app'
    & runError' @ConfigParseError simpleHandler
    & runError' @IllformedPath simpleHandler
    & runError' @ExprParseError simpleHandler
    & runError' @MustacheError simpleHandler
    & runError' @TemplateNotFound simpleHandler
    & runError' @ProjectAlreadExist simpleHandler
    & runTimeC
    & runTerminal

runError' :: forall e m a. Applicative m => (e -> m a) -> ErrorC e m a -> m a
runError' handler = runError handler pure

app' ::
  ( Has (Lift IO) sig m,
    Has (Throw ConfigParseError) sig m,
    Has (Throw IllformedPath) sig m,
    Has (Throw ExprParseError) sig m,
    Has (Throw MustacheError) sig m,
    Has (Throw TemplateNotFound) sig m,
    Has (Throw ProjectAlreadExist) sig m,
    Has Terminal sig m,
    Has Time sig m,
    MonadIO m
  ) =>
  m ()
app' = do
  command <- parseCliOptions
  case command of
    List {..} -> do
      templates <- getTemplates
      let doc = prettyTemplates verbose templates
      prettyPrint stdout doc
    Init {..} -> do
      projectPath <- parseProject project
      config <- getConfig
      mtemplate <- getTemplate template
      case mtemplate of
        Nothing -> throwError $ TemplateNotFound template
        Just tmpl ->
          case tmpl of
            Broken {..} -> throwError $ ConfigParseError (Template name) errors
            Local {..} -> do
              projectExist <- sendIO $ doesDirExist projectPath
              when projectExist $ do
                unless force $ throwError $ ProjectAlreadExist project
                sendIO $ removeDirRecur projectPath
              ctx <- buildContext config templateConfig ops project
              ignores <- ignoredFiles ctx templateConfig
              let mustacheCtx = fromContext ctx
              initFromTemplate ignores mustacheCtx path projectPath
              case license config of
                Just license' -> initializeLicense license' ctx projectPath
                Nothing -> pure ()
