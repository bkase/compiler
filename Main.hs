{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Compiler.ABT as ABT
import qualified Data.List as DL
import qualified Data.Set as Set ()
import Relude (($), ($>), (++), (-), (.), (/=), (<$>), (<*), (<*>), (<=), (<>), (<|>), (==), NonEmpty ((:|)))
import qualified Relude as R
import Text.Megaparsec (Parsec, PosState (..), SourcePos (..), State (..), Stream (..), chunk, empty, getParserState, getSourcePos, many, runParser, satisfy)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Show.Deriving

data Tok
  = Comment R.Text
  | Equals
  | Word R.Text
  | Colon
  | Dot
  | LParen
  | RParen
  | Arrow
  | Slash
  | Num R.Int
  deriving (R.Show, R.Ord, R.Eq)

showMyToken :: Tok -> R.String
showMyToken = \case
  (Comment text) -> "/*" <> R.toString text <> "*/"
  Equals -> "="
  (Word text) -> R.toString text
  Colon -> ":"
  Dot -> "."
  LParen -> "("
  RParen -> ")"
  Arrow -> "->"
  Slash -> "\\"
  (Num n) -> R.show n

type Parser = Parsec R.Void R.Text

data Span = Span {_spanStart :: SourcePos, _spanEnd :: SourcePos}
  deriving (R.Show, R.Ord, R.Eq)

data Span' = Span' {_spanTokenLength :: R.Int, _spanSpan :: Span}
  deriving (R.Show, R.Ord, R.Eq)

data Spanned a = Spanned {_spannedSpan :: Span', _spannedA :: a}
  deriving (R.Show, R.Ord, R.Eq)

spanned :: Parser a -> Parser (Spanned a)
spanned p = do
  startPos <- getSourcePos
  startState <- getParserState
  (x, endPos, endState) <- (,,) <$> p <*> (getSourcePos <* space) <*> getParserState
  let length = stateOffset endState - stateOffset startState
  R.pure (Spanned (Span' length (Span startPos endPos)) x)

doLex :: R.Text -> R.Either (ParseErrorBundle R.Text R.Void) [Spanned Tok]
doLex = runParser tokens "test.pcf"
  where
    tokens :: Parser [Spanned Tok]
    tokens = do
      () <- space
      many $ spanned token
    token :: Parser Tok
    token = comment <|> equals <|> word <|> colon <|> dot <|> lparen <|> rparen <|> arrow <|> slash <|> num
    comment :: Parser Tok
    comment = empty
    equals :: Parser Tok
    equals = char '=' $> Equals
    word :: Parser Tok
    word = do
      first <- letterChar
      rest <- many (alphaNumChar <|> char '-')
      R.pure $ Word $ R.toText (first : rest)
    colon :: Parser Tok
    colon = char ':' $> Colon
    dot :: Parser Tok
    dot = char '.' $> Dot
    lparen :: Parser Tok
    lparen = char '(' $> LParen
    rparen :: Parser Tok
    rparen = char ')' $> RParen
    arrow :: Parser Tok
    arrow = chunk "->" $> Arrow
    slash :: Parser Tok
    slash = char '\\' $> Slash
    num :: Parser Tok
    num = Num <$> decimal

-- PARSER
data Typ = TBool | TNat | TArr Typ Typ
  deriving (R.Show, R.Eq, R.Ord)

data F a
  = Let a a
  | Lam Typ a
  | App a a
  | Fix Typ a
  | If a a a
  | Pred a
  | Succ a
  | Nat R.Int
  | Boolean R.Bool
  deriving (R.Functor, R.Foldable, R.Show, R.Eq, R.Ord)

$(deriveShow1 ''F)

data Toks = Toks {_toksInput :: R.String, _untoks :: [Spanned Tok]}
  deriving (R.Ord, R.Eq, R.Show)

pxy :: R.Proxy Toks
pxy = R.Proxy

tokensLength' :: R.Proxy s -> NonEmpty (Spanned Tok) -> R.Int
tokensLength' R.Proxy xs = R.sum (_spanTokenLength . _spannedSpan <$> xs)

instance Stream Toks where

  type Token Toks = Spanned Tok

  type Tokens Toks = [Spanned Tok]

  tokenToChunk R.Proxy x = [x]

  tokensToChunk R.Proxy xs = xs

  chunkToTokens R.Proxy = R.id

  chunkLength R.Proxy = R.length

  chunkEmpty R.Proxy = R.null

  take1_ (Toks _ []) = R.Nothing
  take1_ (Toks str (t : ts)) =
    R.Just
      ( t,
        Toks (R.drop (tokensLength' pxy (t :| [])) str) ts
        )

  takeN_ n (Toks str s)
    | n <= 0 = R.Just ([], Toks str s)
    | R.null s = R.Nothing
    | R.otherwise =
      let (x, s') = R.splitAt n s
       in case R.nonEmpty x of
            R.Nothing -> R.Just (x, Toks str s')
            R.Just nex -> R.Just (x, Toks (R.drop (tokensLength' pxy nex) str) s')

  takeWhile_ f (Toks str s) =
    let (x, s') = DL.span f s
     in case R.nonEmpty x of
          R.Nothing -> (x, Toks str s')
          R.Just nex -> (x, Toks (R.drop (tokensLength' pxy nex) str) s')

  showTokens R.Proxy =
    R.intercalate " "
      . R.toList
      . R.fmap (showMyToken . _spannedA)

  reachOffset o PosState {..} =
    ( newSourcePos,
      prefix ++ restOfLine,
      PosState
        { pstateInput = Toks
            { _toksInput = postStr,
              _untoks = post
              },
          pstateOffset = R.max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = prefix
          }
      )
    where
      prefix =
        if sameLine
          then pstateLinePrefix ++ preStr
          else preStr
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> pstateSourcePos
          (x : _) -> (_spanStart . _spanSpan . _spannedSpan) x
      (pre, post) = R.splitAt (o - pstateOffset) (_untoks pstateInput)
      (preStr, postStr) = R.splitAt tokensConsumed (_toksInput pstateInput)
      tokensConsumed =
        case R.nonEmpty pre of
          R.Nothing -> 0
          R.Just nePre -> tokensLength' pxy nePre
      restOfLine = R.takeWhile (/= '\n') postStr

type AnnTerm a = ABT.Term F a

type Term' = AnnTerm ()

type Parser2 = Parsec R.Void Toks

type Term = AnnTerm Span

doParse :: Toks -> R.Either (ParseErrorBundle Toks R.Void) Term
doParse = runParser expr "test.pcf"
  where
    expr :: Parser2 Term
    expr = parenned <|> literalBoolean <|> literalNat <|> lam <|> if_ <|> let_
    parenned :: Parser2 Term
    parenned = empty -- between (single LParen) expr (single RParen)
    literalBoolean :: Parser2 Term
    literalBoolean = boolLit "true" R.True <|> boolLit "false" R.False
    boolLit :: R.Text -> R.Bool -> Parser2 Term
    boolLit text b = do
      Spanned (Span' _ span) _ <- satisfy (\(Spanned _ a) -> a == Word text)
      R.pure $ ABT.tm' span (Boolean b)
    lam = empty
    if_ = empty
    let_ = empty
    literalNat = empty

main :: R.IO ()
main = do
  let input = "true"
  case doLex input of
    (R.Left e) -> R.print e
    (R.Right tokens) ->
      R.print $ doParse (Toks (R.toString input) tokens)

-- doLex "fix (\\rec : Nat -> Nat. \\x : Nat. if is-zero x then 0 else rec (pred x)) 2"
