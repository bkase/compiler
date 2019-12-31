{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Compiler.ABT as ABT
import Control.Lens ((.~), (^.))
import Control.Monad.Writer (MonadWriter, WriterT, listen, runWriterT, tell)
import qualified Data.List as DL
import qualified Data.Set as Set (empty)
import Relude (($), ($>), (&), (++), (-), (.), (/=), (<$>), (<*), (<*>), (<=), (<>), (<|>), (==), NonEmpty ((:|)))
import qualified Relude as R
import Text.Megaparsec (Parsec, Pos, PosState (..), SourcePos (..), State (..), Stream (..), between, chunk, empty, getParserState, getSourcePos, initialPos, many, mkPos, runParser, token)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.RawString.QQ
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

instance R.Semigroup Span where
  Span start end <> Span start' end' = Span (start `minStart` start') (end `maxEnd` end')
    where
      coalesce :: (Pos -> Pos -> Pos) -> SourcePos -> SourcePos -> SourcePos
      coalesce op (SourcePos "" l1 c1) (SourcePos n2 l2 c2) = SourcePos n2 (l1 `op` l2) (c1 `op` c2)
      coalesce op (SourcePos n1 l1 c1) (SourcePos "" l2 c2) = SourcePos n1 (l1 `op` l2) (c1 `op` c2)
      coalesce op (SourcePos n1 l1 c1) (SourcePos _ l2 c2) = SourcePos n1 (l1 `op` l2) (c1 `op` c2)
      minStart :: SourcePos -> SourcePos -> SourcePos
      minStart = coalesce R.min
      maxEnd :: SourcePos -> SourcePos -> SourcePos
      maxEnd = coalesce R.max

instance R.Monoid Span where
  mempty = Span (SourcePos "" infPos infPos) (initialPos "")
    where
      infPos :: Pos
      infPos = mkPos R.maxInt

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

type Parser2 = WriterT Span (Parsec R.Void Toks)

type Term = AnnTerm Span

token' :: (Tok -> R.Maybe a) -> Parser2 (Span, a)
token' f = token (\(Spanned (Span' _ span) x) -> (span,) <$> f x) Set.empty

satisfy' :: (Tok -> R.Bool) -> Parser2 (Span, Tok)
satisfy' p = token' (\tok -> if p tok then R.Just tok else R.Nothing)

flush :: Parser2 (Span, a) -> Parser2 a
flush p = do
  (span, a) <- p
  () <- tell span
  R.pure a

annotate :: Parser2 (Span, Term) -> Parser2 Term
annotate p = do
  (span, term) <- p
  () <- tell span
  R.pure $ term & ABT.termAnnotation .~ span

examine :: Parser2 Term -> Parser2 (Span, Term)
examine p = do
  term <- p
  R.pure (term ^. ABT.termAnnotation, term)

listen' :: MonadWriter w m => m a -> m (w, a)
listen' ma = R.swap <$> listen ma

doParse :: Toks -> R.Either (ParseErrorBundle Toks R.Void) Term
doParse toks = R.fst <$> partial toks
  where
    partial :: Toks -> R.Either (ParseErrorBundle Toks R.Void) (Term, Span)
    partial = runParser (runWriterT expr) "test.pcf"
    expr :: Parser2 Term
    expr = parenned <|> literalBoolean <|> literalNat <|> lam <|> if_ <|> let_
    parenned :: Parser2 Term
    parenned =
      between
        (satisfy' (== LParen) & flush)
        (satisfy' (== RParen) & flush)
        expr
        & listen'
        & annotate
    literalBoolean :: Parser2 Term
    literalBoolean = boolLit "true" R.True <|> boolLit "false" R.False
    boolLit :: R.Text -> R.Bool -> Parser2 Term
    boolLit text b = R.fmap (R.const (ABT.tm (Boolean b))) <$> satisfy' (\a -> a == Word text) & annotate
    lam = empty
    if_ = annotate $ listen' $ do
      _ <- satisfy' (== Word "if") & flush
      e1 <- expr
      _ <- satisfy' (== Word "then") & flush
      e2 <- expr
      _ <- satisfy' (== Word "else") & flush
      e3 <- expr
      R.pure $ ABT.tm (If e1 e2 e3)
    let_ = empty
    literalNat = empty

main :: R.IO ()
main = do
  let input =
        [r|
      if ((false)) then
          true
      else
          (true)
  |]
  case doLex input of
    (R.Left e) -> R.print e
    (R.Right tokens) ->
      R.print $ doParse (Toks (R.toString input) tokens)

-- doLex "fix (\\rec : Nat -> Nat. \\x : Nat. if is-zero x then 0 else rec (pred x)) 2"
