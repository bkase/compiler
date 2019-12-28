{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Relude (($), ($>), (<$>), (<*), (<*>), (<|>))
import qualified Relude as R
import Text.Megaparsec (Parsec, SourcePos, chunk, empty, getSourcePos, many, runParser)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)

data Token
  = Comment R.Text
  | Equals
  | Word R.Text
  | Colon
  | Dot
  | LParen
  | RParen
  | Arrow
  | Slash
  | Nat R.Int
  deriving (R.Show)

type Parser = Parsec R.Void R.Text

data Span = Span SourcePos SourcePos
  deriving (R.Show)

data Spanned a = Spanned Span a
  deriving (R.Show)

spanned :: Parser (a, SourcePos) -> Parser (Spanned a)
spanned parser = do
  start <- getSourcePos
  (x, end) <- parser
  R.pure (Spanned (Span start end) x)

lexeme :: Parser a -> Parser (a, SourcePos)
lexeme p = (,) <$> p <*> (getSourcePos <* space)

doLex :: R.Text -> R.Either (ParseErrorBundle R.Text R.Void) [Spanned Token]
doLex = runParser tokens "test.pcf"
  where
    tokens :: Parser [Spanned Token]
    tokens = do
      () <- space
      many $ spanned $ lexeme token
    token :: Parser Token
    token = comment <|> equals <|> word <|> colon <|> dot <|> lparen <|> rparen <|> arrow <|> slash <|> nat
    comment :: Parser Token
    comment = empty
    equals :: Parser Token
    equals = char '=' $> Equals
    word :: Parser Token
    word = do
      first <- letterChar
      rest <- many (alphaNumChar <|> char '-')
      R.pure $ Word $ R.toText (first : rest)
    colon :: Parser Token
    colon = char ':' $> Colon
    dot :: Parser Token
    dot = char '.' $> Dot
    lparen :: Parser Token
    lparen = char '(' $> LParen
    rparen :: Parser Token
    rparen = char ')' $> RParen
    arrow :: Parser Token
    arrow = chunk "->" $> Arrow
    slash :: Parser Token
    slash = char '\\' $> Slash
    nat :: Parser Token
    nat = Nat <$> decimal

main :: R.IO ()
main = R.print $ doLex "fix (\\rec : Nat -> Nat. \\x : Nat. if is-zero x then 0 else rec (pred x)) 2"
