{-# LANGUAGE QuasiQuotes #-}

module HI.Cli where

import Control.Effect.Lift
import Control.Effect.Terminal
import Control.Effect.Throw
import Data.Maybe
import Data.String.Interpolate
import Data.Text (Text, unpack)
import Data.Text.Prettyprint.Doc
import HI.Errors
import HI.Template
import HI.Types
import HI.Utils
import Path
import Prettyprinter.Render.Terminal
import System.IO

query :: Has Terminal sig m => ValType -> Text -> Maybe Text -> m Val
query ty name desc = do
  prettyPrint stdout info
  query' ty
  where
    info :: Doc AnsiStyle
    info =
      vsep
        [ "Please input the value of option",
          indent 4 $
            hsep $
              pure (annotate (color Cyan <> underlined) [i|#{name} : #{ty}|])
                ++ fmap (parens . pretty) (maybeToList desc)
        ]

query' :: Has Terminal sig m => ValType -> m Val
query' ty = do
  resp <- prompt "(y/n)> "
  case resp of
    Nothing -> query' ty
    Just r ->
      case ty of
        Text' -> pure $ Text r
        Bool' ->
          case readYesNo r of
            Nothing -> do
              prettyPrint stderr $ annotate (color Red) [i|Please type "y" or "n"|]
              query' ty
            Just b -> pure $ Bool b

parseProject :: Has (Throw IllformedPath) sig m => Text -> m (Path Rel Dir)
parseProject project =
  let err = ProjectName project
   in case parseRelDir (unpack project) of
        Nothing -> throwError err
        Just dir
          | not (underCurrentDir dir) -> throwError err
          | otherwise -> pure dir
