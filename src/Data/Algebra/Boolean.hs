{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

-- | Boolean expressions implemented using naive trees
module Data.Algebra.Boolean
  ( Parser,
    CTX,
    Expr,
    eval,
    expr,
  )
where

import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Data.Char
import Data.Foldable
import Data.List.NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text, unpack)
import Data.Void
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

newtype Var = Var Text
  deriving newtype (Show, Eq, Ord)

-- | Context. Every variable not in context will be treated as False.
type CTX = Map Text Bool

data Expr
  = BNeg Expr
  | BVar Var
  | BConj Expr Expr
  | BDisj Expr Expr
  deriving (Eq)

instance Show Expr where
  showsPrec _ (BVar (Var v)) = showString $ unpack v
  showsPrec p (BNeg expr') =
    showParen (p > 3) $
      showString "¬" . showsPrec 2 expr'
  showsPrec p (BConj expr1 expr2) =
    showParen (p > 2) $
      showsPrec 2 expr1 . showString " ∧ " . showsPrec 2 expr2
  showsPrec p (BDisj expr1 expr2) =
    showParen (p > 1) $
      showsPrec 1 expr1 . showString " ∨ " . showsPrec 1 expr2

-- | Evaluate an expression in a context.
eval :: CTX -> Expr -> Bool
eval ctx e
  | BNeg e' <- e = not $ eval ctx e'
  | BVar v <- e = lkup v
  | BConj e1 e2 <- e = eval ctx e1 && eval ctx e2
  | BDisj e1 e2 <- e = eval ctx e1 || eval ctx e2
  where
    lkup :: Var -> Bool
    lkup (Var v) = Just True == M.lookup v ctx

symbol :: Text -> Parser Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

conj :: Parser Char
conj = lexeme $ single '&' <|> single '∧'

disj :: Parser Char
disj = lexeme $ single '|' <|> single '∨'

neg :: Parser Char
neg = lexeme $ single '!' <|> single '¬'

conjList :: NonEmpty Expr -> Expr
conjList (h :| l) = foldl' BConj h l

disjList :: NonEmpty Expr -> Expr
disjList (h :| l) = foldl' BDisj h l

neg' :: Parser Expr
neg' = try negated <|> normal
  where
    negated = do
      _ <- neg
      BNeg <$> term
    normal = term

conj' :: Parser Expr
conj' = conjList <$> sepBy1 neg' conj

var :: Parser Expr
var = lexeme $ BVar . Var <$> takeWhile1P (Just "variable") isAlphaNum

term :: Parser Expr
term = paren expr <|> var

-- | Parse an expression
expr :: Parser Expr
expr = disjList <$> sepBy1 conj' disj
