{-# LANGUAGE OverloadedStrings #-}
{-
Orphaned instances usually is not good, but I use them here for semantics and simplicity reasons.
We can always introduce newtype wrappers to avoid them or declare instances directly in corresponding modules.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HW3.Pretty (prettyValue, prettyError) where

import qualified Data.ByteString as B
import Data.Foldable (Foldable (toList))
import qualified Data.Map.Strict as Map
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import GHC.Real (Ratio ((:%)))
import HW3.Base (HiAction (..), HiError (..), HiFun (..), HiValue (..))
import Numeric (showHex)
import Prettyprinter (Doc, Pretty, comma, dquotes, encloseSep, hsep, parens, pretty, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty

prettyError :: HiError -> Doc AnsiStyle
prettyError = pretty

instance Pretty HiError where
  pretty HiErrorInvalidArgument = "Error: Invalid argument"
  pretty HiErrorInvalidFunction = "Error: Invalid function"
  pretty HiErrorArityMismatch = "Error: Arity mismatch"
  pretty HiErrorDivideByZero = "Error: Division by zero"

instance Pretty HiFun where
  pretty HiFunNotLessThan = "not-less-than"
  pretty HiFunNotGreaterThan = "not-greater-than"
  pretty HiFunNotEquals = "not-equals"
  pretty HiFunLessThan = "less-than"
  pretty HiFunGreaterThan = "greater-than"
  pretty HiFunEquals = "equals"
  pretty HiFunNot = "not"
  pretty HiFunAnd = "and"
  pretty HiFunOr = "or"
  pretty HiFunIf = "if"
  pretty HiFunDiv = "div"
  pretty HiFunMul = "mul"
  pretty HiFunAdd = "add"
  pretty HiFunSub = "sub"
  pretty HiFunLength = "length"
  pretty HiFunToUpper = "to-upper"
  pretty HiFunToLower = "to-lower"
  pretty HiFunReverse = "reverse"
  pretty HiFunTrim = "trim"
  pretty HiFunList = "list"
  pretty HiFunRange = "range"
  pretty HiFunFold = "fold"
  pretty HiFunPackBytes = "pack-bytes"
  pretty HiFunUnpackBytes = "unpack-bytes"
  pretty HiFunEncodeUtf8 = "encode-utf8"
  pretty HiFunDecodeUtf8 = "decode-utf8"
  pretty HiFunZip = "zip"
  pretty HiFunUnzip = "unzip"
  pretty HiFunSerialise = "serialise"
  pretty HiFunDeserialise = "deserialise"
  pretty HiFunRead = "read"
  pretty HiFunWrite = "write"
  pretty HiFunMkDir = "mkdir"
  pretty HiFunChDir = "cd"
  pretty HiFunParseTime = "parse-time"
  pretty HiFunRand = "rand"
  pretty HiFunEcho = "echo"
  pretty HiFunCount = "count"
  pretty HiFunKeys = "keys"
  pretty HiFunValues = "values"
  pretty HiFunInvert = "invert"

instance Pretty HiAction where
  pretty (HiActionRead fp) = "read" <> parens (dquotes $ pretty fp)
  pretty (HiActionWrite fp bs) = "write" <> parens (dquotes (pretty fp) <> ", " <> pretty bs)
  pretty (HiActionMkDir fp) = "mkdir" <> parens (dquotes $ pretty fp)
  pretty (HiActionChDir fp) = "cd" <> parens (dquotes $ pretty fp)
  pretty HiActionCwd = "cwd"
  pretty HiActionNow = "now"
  pretty (HiActionRand l r) = "rand(" <> pretty l <> ", " <> pretty r <> ")"
  pretty (HiActionEcho t) = "echo(" <> viaShow t <> ")"

instance Pretty HiValue where
  pretty (HiValueBool True) = "true"
  pretty (HiValueBool False) = "false"
  pretty (HiValueNumber num@(n :% d)) =
    let (q, r) = quotRem n d
     in if r == 0
          then pretty q
          else case fromRationalRepetendUnlimited num of
            (sci, Nothing) -> pretty $ formatScientific Fixed Nothing sci
            _ ->
              if q == 0
                then pretty n <> "/" <> pretty d
                else
                  pretty q
                    <+> (if r > 0 then "+" <+> pretty r else "-" <+> pretty (negate r))
                    <> "/"
                    <> pretty d
  pretty (HiValueFunction f) = pretty f
  pretty HiValueNull = "null"
  pretty (HiValueString t) = viaShow t
  pretty (HiValueList lst) = pretty $ toList lst
  pretty (HiValueBytes bs) = pretty bs
  pretty (HiValueAction act) = pretty act
  pretty (HiValueTime time) = "parse-time" <> parens (dquotes (viaShow time))
  pretty (HiValueDict dict) = encloseSep "{ " " }" comma (prettyKeyValue <$> Map.assocs dict)

instance Pretty B.ByteString where
  pretty bs = "[#" <+> hsep (map (\w8 -> prependHex $ showHex w8 "") (B.unpack bs)) <+> "#]"

prettyKeyValue :: (HiValue, HiValue) -> Doc ann
prettyKeyValue (a, b) = pretty a <> ": " <> pretty b

prependHex :: String -> Doc ann
prependHex s = pretty $ if length s < 2 then "0" ++ s else s
