module Parser
  ( parseSheet
  , pSheet
  , pCell
  , pContent
  , pExpr
  , pAddr
  , pValue
  ) where

import Data.Void (Void)
import qualified Data.Map as Map
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import Types

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--" <|> L.skipLineComment "//")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pAddr :: Parser Addr
pAddr = lexeme $ do
  cols <- some upperChar
  rows <- L.decimal
  return (cols, rows)

pValue :: Parser Value
pValue = lexeme (pBool <|> pNum <|> pStr)
  where
    pBool = (BoolV True <$ symbol "TRUE") <|> (BoolV False <$ symbol "FALSE")
    pStr = StrV <$> (char '"' *> manyTill L.charLiteral (char '"'))
    pNum = NumV <$> L.signed sc (try L.float <|> (fromIntegral <$> (L.decimal :: Parser Integer)))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pRangeOpExpr :: Parser Expr
pRangeOpExpr = do
  op <- (SumR <$ symbol "SUM") <|> (AvgR <$ symbol "AVG")
  _ <- symbol "("
  start <- pAddr
  _ <- symbol ":"
  end <- pAddr
  _ <- symbol ")"
  return $ RangeOp op start end

pTerm :: Parser Expr
pTerm = choice
  [ try pRangeOpExpr
  , LitE <$> try pValue
  , Ref <$> try pAddr
  , parens pExpr
  ]

operators :: [[Operator Parser Expr]]
operators =
  [ [ InfixL (BinOp Mul <$ symbol "*")
    , InfixL (BinOp Div <$ symbol "/")
    ]
  , [ InfixL (BinOp Add <$ symbol "+")
    , InfixL (BinOp Sub <$ symbol "-")
    ]
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operators

pContent :: Parser Content
pContent = do
  expr <- pExpr
  case expr of
    LitE val -> return (Lit val)
    _        -> return (Form expr)

pCell :: Parser Cell
pCell = do
  addr' <- pAddr
  _ <- symbol "="
  content' <- pContent
  _ <- symbol ";"
  return $ Cell addr' content'

pSheet :: Parser Sheet
pSheet = do
  _ <- sc
  _ <- symbol "sheet"
  _ <- symbol "{"
  cells <- many pCell
  _ <- symbol "}"
  _ <- eof
  return $ Sheet (Map.fromList [(addr c, c) | c <- cells])

parseSheet :: String -> Either String Sheet
parseSheet input = case parse pSheet "" input of
  Left err  -> Left (errorBundlePretty err)
  Right val -> Right val
