module HW3.Parser (parse) where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR), makeExprParser)
import qualified Data.ByteString as B
import Data.Char (digitToInt, isAlpha, isAlphaNum)
import Data.Foldable (foldl')
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Word (Word8)
import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import Text.Megaparsec (ParseErrorBundle, Parsec, between, choice, count, empty, eof, many, manyTill, notFollowedBy, option, runParser, satisfy, sepBy, sepBy1)
import Text.Megaparsec.Char (char, hexDigitChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parseHiFun :: Parser HiFun
parseHiFun =
  choice
    [ HiFunNotLessThan <$ symbol "not-less-than",
      HiFunNotGreaterThan <$ symbol "not-greater-than",
      HiFunNotEquals <$ symbol "not-equals",
      HiFunLessThan <$ symbol "less-than",
      HiFunGreaterThan <$ symbol "greater-than",
      HiFunEquals <$ symbol "equals",
      HiFunNot <$ symbol "not",
      HiFunAnd <$ symbol "and",
      HiFunOr <$ symbol "or",
      HiFunIf <$ symbol "if",
      HiFunDiv <$ symbol "div",
      HiFunMul <$ symbol "mul",
      HiFunAdd <$ symbol "add",
      HiFunSub <$ symbol "sub",
      HiFunLength <$ symbol "length",
      HiFunToUpper <$ symbol "to-upper",
      HiFunToLower <$ symbol "to-lower",
      HiFunReverse <$ symbol "reverse",
      HiFunTrim <$ symbol "trim",
      HiFunList <$ symbol "list",
      HiFunRange <$ symbol "range",
      HiFunFold <$ symbol "fold",
      HiFunPackBytes <$ symbol "pack-bytes",
      HiFunUnpackBytes <$ symbol "unpack-bytes",
      HiFunEncodeUtf8 <$ symbol "encode-utf8",
      HiFunDecodeUtf8 <$ symbol "decode-utf8",
      HiFunZip <$ symbol "zip",
      HiFunUnzip <$ symbol "unzip",
      HiFunSerialise <$ symbol "serialise",
      HiFunDeserialise <$ symbol "deserialise",
      HiFunRead <$ symbol "read",
      HiFunWrite <$ symbol "write",
      HiFunMkDir <$ symbol "mkdir",
      HiFunChDir <$ symbol "cd",
      HiFunParseTime <$ symbol "parse-time",
      HiFunRand <$ symbol "rand",
      HiFunEcho <$ symbol "echo",
      HiFunCount <$ symbol "count",
      HiFunKeys <$ symbol "keys",
      HiFunValues <$ symbol "values",
      HiFunInvert <$ symbol "invert"
    ]

parseHiValueBool :: Parser Bool
parseHiValueBool =
  choice
    [ True <$ symbol "true",
      False <$ symbol "false"
    ]

parseHiValueNumber :: Parser Rational
parseHiValueNumber = do
  number <- lexeme $ L.signed sc L.scientific
  return $ toRational number

parseHiValueString :: Parser T.Text
parseHiValueString = do
  text <- lexeme $ char '"' >> manyTill L.charLiteral (char '"')
  return $ T.pack text

parseByte :: Parser Word8
parseByte = do
  hex <- lexeme $ count 2 hexDigitChar <* notFollowedBy hexDigitChar
  return $ fromIntegral $ foldl' (\acc c -> acc * 16 + digitToInt c) 0 hex

parseHiValueBytes :: Parser B.ByteString
parseHiValueBytes = do
  bytes <- between (symbol "[#") (symbol "#]") (many parseByte)
  return $ B.pack bytes

parseHiValueAction :: Parser HiAction
parseHiValueAction =
  choice
    [ HiActionCwd <$ symbol "cwd",
      HiActionNow <$ symbol "now"
    ]

parseHiValue :: Parser HiValue
parseHiValue =
  choice
    [ HiValueBool <$> parseHiValueBool,
      HiValueNumber <$> parseHiValueNumber,
      HiValueFunction <$> parseHiFun,
      HiValueNull <$ symbol "null",
      HiValueString <$> parseHiValueString,
      HiValueBytes <$> parseHiValueBytes,
      HiValueAction <$> parseHiValueAction
    ]

parseList :: Parser HiExpr
parseList = do
  elems <- between (symbol "[") (symbol "]") (sepBy parseHiExprInfix (symbol ","))
  return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) elems

parseKeyValue :: Parser (HiExpr, HiExpr)
parseKeyValue = do
  key <- parseHiExprInfix
  _ <- symbol ":"
  value <- parseHiExprInfix
  return (key, value)

parseHiExprDict :: Parser [(HiExpr, HiExpr)]
parseHiExprDict = between (symbol "{") (symbol "}") (sepBy parseKeyValue (symbol ","))

parseArgs :: Parser [HiExpr]
parseArgs = between (symbol "(") (symbol ")") (sepBy parseHiExprInfix (symbol ","))

parseDotAccess :: Parser String
parseDotAccess = do
  field <- lexeme $ ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
  return $ intercalate "-" field

parseHiExprCont :: HiExpr -> Parser HiExpr
parseHiExprCont e =
  choice
    [ do
        args <- parseArgs
        let apply = HiExprApply e args
         in option apply (parseHiExprCont apply),
      do
        _ <- symbol "!"
        let run = HiExprRun e
         in option run (parseHiExprCont run),
      do
        _ <- symbol "."
        field <- parseDotAccess
        let apply = HiExprApply e [HiExprValue $ HiValueString $ T.pack field]
         in option apply (parseHiExprCont apply)
    ]

parseHiExpr :: Parser HiExpr
parseHiExpr = do
  expr <- choice [HiExprValue <$> parseHiValue, parseList, HiExprDict <$> parseHiExprDict]
  option expr (parseHiExprCont expr)

applyHiFun :: HiFun -> HiExpr -> HiExpr -> HiExpr
applyHiFun f a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]

hiFunOp :: HiFun -> Parser String -> Parser (HiExpr -> HiExpr -> HiExpr)
hiFunOp f parser = applyHiFun f <$ parser

table :: [[Operator Parser HiExpr]]
table =
  [ [ InfixL $ hiFunOp HiFunMul $ symbol "*",
      InfixL $ hiFunOp HiFunDiv $ notFollowedBy (symbol "/=") >> symbol "/"
    ],
    [ InfixL $ hiFunOp HiFunAdd $ symbol "+",
      InfixL $ hiFunOp HiFunSub $ symbol "-"
    ],
    [ InfixN $ hiFunOp HiFunNotLessThan $ symbol ">=",
      InfixN $ hiFunOp HiFunNotGreaterThan $ symbol "<=",
      InfixN $ hiFunOp HiFunLessThan $ symbol "<",
      InfixN $ hiFunOp HiFunGreaterThan $ symbol ">",
      InfixN $ hiFunOp HiFunEquals $ symbol "==",
      InfixN $ hiFunOp HiFunNotEquals $ symbol "/="
    ],
    [InfixR $ hiFunOp HiFunAnd $ symbol "&&"],
    [InfixR $ hiFunOp HiFunOr $ symbol "||"]
  ]

term :: Parser HiExpr
term =
  choice
    [ parseHiExpr,
      do
        expr <- between (symbol "(") (symbol ")") parseHiExprInfix
        option expr (parseHiExprCont expr)
    ]

parseHiExprInfix :: Parser HiExpr
parseHiExprInfix = makeExprParser term table

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (sc >> lexeme parseHiExprInfix <* eof) empty
