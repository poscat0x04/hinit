{-# LANGUAGE QuasiQuotes #-}

module Hinit.Types where

import Data.Map.Strict (Map)
import Data.String.Interpolate
import Data.Text (Text)
import GHC.Generics
import Text.Mustache
import qualified Text.Mustache.Types as M
import Toml hiding (Bool, Text)
import qualified Toml as T

type Context = Map Text Val

contextCodec :: TomlCodec Context
contextCodec = tableMap _KeyText valCodec "custom"

data ValType
  = Bool'
  | Text'
  deriving (Eq, Generic)

instance Show ValType where
  show Bool' = "Bool"
  show Text' = "Text"

data Val
  = Bool Bool
  | Text Text
  deriving (Show, Eq, Generic)

instance ToMustache Val where
  toMustache (Bool b) = M.Bool b
  toMustache (Text t) = M.String t

toTomlVal :: Val -> AnyValue
toTomlVal (Bool b) = AnyValue $ T.Bool b
toTomlVal (Text t) = AnyValue $ T.Text t

fromTomlVal :: AnyValue -> Either TomlBiMapError Val
fromTomlVal (AnyValue v)
  | T.Bool b <- v = Right $ Bool b
  | T.Text t <- v = Right $ Text t
  | otherwise =
    Left $
      ArbitraryError
        [i|Expecting value of type Bool or Text, got #{tShow (valueType v)} instead|]

_Val :: TomlBiMap Val AnyValue
_Val = invert $ prism toTomlVal fromTomlVal

valCodec :: Key -> TomlCodec Val
valCodec = match _Val

instance HasCodec Val where
  hasCodec = valCodec

data VCS
  = Git
  | Mercurial
  | Darcs
  | Pijul
  | Other Text
  deriving (Show, Eq, Generic)

vcsToT :: VCS -> Text
vcsToT v =
  case v of
    Git -> "Git"
    Mercurial -> "Mercurial"
    Darcs -> "Darcs"
    Pijul -> "Pijul"
    Other t -> t

textToVcs :: Text -> VCS
textToVcs "Git" = Git
textToVcs "Mercurial" = Mercurial
textToVcs "Darcs" = Darcs
textToVcs "Pijul" = Pijul
textToVcs t = Other t

vcsCodec :: Key -> TomlCodec (Maybe VCS)
vcsCodec k = dioptional $ fmap textToVcs (text k) .= vcsToT

instance HasCodec (Maybe VCS) where
  hasCodec = vcsCodec
