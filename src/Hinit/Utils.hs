module Hinit.Utils where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.FilePath.Glob

mkBulletList :: [Doc a] -> Doc a
mkBulletList = vsep . map (("•" <+>) . nest 2)

mkError :: Doc AnsiStyle -> Doc AnsiStyle
mkError doc = annotate (color Red) "Error:" <+> nest 2 doc

readYesNo :: Text -> Maybe Bool
readYesNo s =
  if
      | s == "Y" || s == "y" || s == "Yes" || s == "yes" -> pure True
      | s == "N" || s == "n" || s == "No" || s == "no" -> pure False
      | otherwise -> Nothing

newtype PrettyShow a = PrettyShow {unPrettyShow :: a}

instance Pretty a => Show (PrettyShow a) where
  show = show . pretty . unPrettyShow

matches :: [Pattern] -> FilePath -> Bool
matches ps fp = or $ fmap (`match` fp) ps

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just a) k = k a
