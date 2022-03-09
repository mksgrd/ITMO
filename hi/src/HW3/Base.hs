{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module HW3.Base
  ( HiFun (..),
    HiValue (..),
    HiExpr (..),
    HiError (..),
    HiAction (..),
    HiMonad,
    runAction,
    arity,
  )
where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data HiFun -- function names (e.g. div, sort, length, ...)
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

data HiValue -- values (numbers, booleans, strings, ...)
  = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

data HiExpr -- expressions (literals, function calls, ...)
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq, Ord)

data HiError -- evaluation errors (invalid arguments, ...)
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq, Ord)

data HiAction
  = HiActionRead FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

arity :: HiFun -> Int
arity HiFunDiv = 2
arity HiFunMul = 2
arity HiFunAdd = 2
arity HiFunSub = 2
arity HiFunNot = 1
arity HiFunAnd = 2
arity HiFunOr = 2
arity HiFunLessThan = 2
arity HiFunGreaterThan = 2
arity HiFunEquals = 2
arity HiFunNotLessThan = 2
arity HiFunNotGreaterThan = 2
arity HiFunNotEquals = 2
arity HiFunIf = 3
arity HiFunLength = 1
arity HiFunToUpper = 1
arity HiFunToLower = 1
arity HiFunReverse = 1
arity HiFunTrim = 1
arity HiFunList = maxBound
arity HiFunRange = 2
arity HiFunFold = 2
arity HiFunPackBytes = 1
arity HiFunUnpackBytes = 1
arity HiFunEncodeUtf8 = 1
arity HiFunDecodeUtf8 = 1
arity HiFunZip = 1
arity HiFunUnzip = 1
arity HiFunSerialise = 1
arity HiFunDeserialise = 1
arity HiFunRead = 1
arity HiFunWrite = 2
arity HiFunMkDir = 1
arity HiFunChDir = 1
arity HiFunParseTime = 1
arity HiFunRand = 2
arity HiFunEcho = 1
arity HiFunCount = 1
arity HiFunKeys = 1
arity HiFunValues = 1
arity HiFunInvert = 1
