{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Hinit.Template.Config where

import Control.Applicative
import Data.String.Interpolate
import Data.Text (Text, pack, unpack)
import Data.Tuple.Optics
import GHC.Generics
import Optics.Operators
import System.FilePath.Glob
  ( Pattern,
    compile,
    decompile,
    simplify,
  )
import Toml
import qualified Toml as T

data TemplateConfig = TemplateConfig
  { desc :: Maybe Text,
    tags :: [Text],
    ignores :: [Pattern],
    options :: [Option],
    optionals :: [OptionalIgnores]
  }
  deriving (Show, Eq, Generic)

pToTomlVal :: Pattern -> AnyValue
pToTomlVal p = AnyValue $ Text $ pack $ decompile p

tomlValToP :: AnyValue -> Either TomlBiMapError Pattern
tomlValToP (AnyValue v)
  | Text t <- v = Right $ simplify $ compile $ unpack t
  | otherwise = Left $ WrongValue $ MatchError TText (AnyValue v)

_Pattern :: TomlBiMap Pattern AnyValue
_Pattern = invert $ prism pToTomlVal tomlValToP

instance HasItemCodec Pattern where
  hasItemCodec = Left _Pattern

templateConfigCodec :: TomlCodec TemplateConfig
templateConfigCodec = genericCodec

data Option
  = BoolOpt
      { name :: Text,
        desc :: Maybe Text,
        defB :: Maybe Bool
      }
  | TextOpt
      { name :: Text,
        desc :: Maybe Text,
        defT :: Maybe Text
      }
  deriving (Show, Eq, Generic)

pair' :: TomlCodec a -> TomlCodec b -> TomlCodec c -> TomlCodec (a, b, c)
pair' a b c =
  (,,)
    <$> a .= (^. _1)
      <*> b .= (^. _2)
      <*> c .= (^. _3)

matchB :: Option -> Maybe (Text, Maybe Text, Bool)
matchB BoolOpt {..} = fmap (name,desc,) defB
matchB _ = Nothing

matchB' :: Option -> Maybe (Text, Maybe Text)
matchB' BoolOpt {..} = Just (name, desc)
matchB' _ = Nothing

matchT :: Option -> Maybe (Text, Maybe Text, Text)
matchT TextOpt {..} = fmap (name,desc,) defT
matchT _ = Nothing

matchT' :: Option -> Maybe (Text, Maybe Text)
matchT' TextOpt {..} = Just (name, desc)
matchT' _ = Nothing

descCodec :: TomlCodec (Maybe Text)
descCodec = dioptional (text "desc")

tyCodec :: Text -> TomlCodec Text
tyCodec ty = match _Ty "type"
  where
    _Ty = invert $ prism (AnyValue . T.Text) tyFromToml
    tyFromToml (AnyValue v)
      | T.Text t <- v =
        if t == ty
          then Right t
          else Left $ ArbitraryError [i|Expecting type #{ty}, got type #{t}|]
      | otherwise =
        Left $ WrongValue $ MatchError TText (AnyValue v)

optCodec :: TomlCodec Option
optCodec =
  dimatch matchB (\(a, b, c) -> BoolOpt a b (Just c)) bOptCodec
    <|> dimatch matchB' (\(a, b) -> BoolOpt a b Nothing) bOptCodec'
    <|> dimatch matchT (\(a, b, c) -> TextOpt a b (Just c)) tOptCodec
    <|> dimatch matchT' (\(a, b) -> BoolOpt a b Nothing) tOptCodec'
  where
    bOptCodec :: TomlCodec (Text, Maybe Text, Bool)
    bOptCodec = pair' (text "name") descCodec (bool "default")
    bOptCodec' :: TomlCodec (Text, Maybe Text)
    bOptCodec' =
      pair (text "name") descCodec
        <* tyCodec "bool" .= const "bool"
    tOptCodec :: TomlCodec (Text, Maybe Text, Text)
    tOptCodec =
      pair' (text "name") descCodec (text "default")
    tOptCodec' :: TomlCodec (Text, Maybe Text)
    tOptCodec' =
      pair (text "name") descCodec
        <* tyCodec "text" .= const "text"

instance HasItemCodec Option where
  hasItemCodec = Right optCodec

data OptionalIgnores = Optional
  { when :: Text,
    ignores :: [Pattern]
  }
  deriving (Show, Eq, Generic)

instance HasItemCodec OptionalIgnores where
  hasItemCodec = Right genericCodec
