{-# LANGUAGE OverloadedStrings #-}

module HW3.Evaluator (eval) where

import Codec.Compression.Zlib (bestCompression, compressLevel, compressWith, decompressWith, defaultCompressParams, defaultDecompressParams)
import Codec.Serialise (deserialise, serialise)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Foldable (foldl', toList)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup (stimes))
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (addUTCTime, diffUTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Word (Word8)
import GHC.Real (denominator, numerator)
import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad, HiValue (..), arity, runAction)
import Text.Read (readMaybe)

safeToInt :: Monad m => Rational -> ExceptT HiError m Int
safeToInt r = do
  unless (denominator r == 1) (throwE HiErrorInvalidArgument)
  let num = numerator r
      lb = minBound :: Int
      ub = maxBound :: Int
   in if num <= fromIntegral ub && num >= fromIntegral lb
        then return $ fromIntegral num
        else throwE HiErrorInvalidArgument

times :: (Monad m, Semigroup a) => (a -> HiValue) -> a -> Rational -> ExceptT HiError m HiValue
times cons sgp n = do
  cnt <- safeToInt n
  unless (cnt > 0) (throwE HiErrorInvalidArgument)
  return $ cons $ stimes cnt sgp

safeToWord8 :: Monad m => HiValue -> ExceptT HiError m Word8
safeToWord8 (HiValueNumber n) = do
  byte <- safeToInt n
  unless (byte >= 0 && byte <= 255) (throwE HiErrorInvalidArgument)
  return $ fromIntegral byte
safeToWord8 _ = throwE HiErrorInvalidArgument

countToDict :: (Monad m, Foldable f) => f a -> (a -> HiValue) -> ExceptT HiError m HiValue
countToDict elems pack =
  return $
    HiValueDict $
      foldl'
        (\m c -> Map.insertWith (\_ (HiValueNumber r) -> HiValueNumber (r + 1)) (pack c) (HiValueNumber 1) m)
        Map.empty
        elems

strictFunApply :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
strictFunApply HiFunDiv [HiValueNumber _, HiValueNumber 0] = throwE HiErrorDivideByZero
strictFunApply HiFunDiv [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber $ a / b
strictFunApply HiFunDiv [HiValueString a, HiValueString b] = return $ HiValueString $ a <> "/" <> b
strictFunApply HiFunMul [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber $ a * b
strictFunApply HiFunMul [HiValueString a, HiValueNumber b] = times HiValueString a b
strictFunApply HiFunMul [HiValueList a, HiValueNumber b] = times HiValueList a b
strictFunApply HiFunMul [HiValueBytes a, HiValueNumber b] = times HiValueBytes a b
strictFunApply HiFunAdd [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber $ a + b
strictFunApply HiFunAdd [HiValueString a, HiValueString b] = return $ HiValueString $ a <> b
strictFunApply HiFunAdd [HiValueList a, HiValueList b] = return $ HiValueList $ a <> b
strictFunApply HiFunAdd [HiValueBytes a, HiValueBytes b] = return $ HiValueBytes $ a <> b
strictFunApply HiFunAdd [HiValueTime t, HiValueNumber n] = return $ HiValueTime $ addUTCTime (secondsToNominalDiffTime $ fromRational n) t
strictFunApply HiFunSub [HiValueTime a, HiValueTime b] = return $ HiValueNumber $ toRational $ nominalDiffTimeToSeconds $ diffUTCTime a b
strictFunApply HiFunSub [HiValueNumber a, HiValueNumber b] = return $ HiValueNumber $ a - b
strictFunApply HiFunNot [HiValueBool b] = return $ HiValueBool $ not b
strictFunApply HiFunEquals [a, b] = return $ HiValueBool $ a == b
strictFunApply HiFunLessThan [a, b] = return $ HiValueBool $ a < b
strictFunApply HiFunGreaterThan [a, b] = return $ HiValueBool $ a > b
strictFunApply HiFunNotEquals [a, b] = return $ HiValueBool $ a /= b
strictFunApply HiFunNotLessThan [a, b] = return $ HiValueBool $ a >= b
strictFunApply HiFunNotGreaterThan [a, b] = return $ HiValueBool $ a <= b
strictFunApply HiFunLength [HiValueString t] = return $ HiValueNumber $ toRational $ T.length t
strictFunApply HiFunLength [HiValueList lst] = return $ HiValueNumber $ toRational $ S.length lst
strictFunApply HiFunLength [HiValueBytes b] = return $ HiValueNumber $ toRational $ B.length b
strictFunApply HiFunToUpper [HiValueString t] = return $ HiValueString $ T.toUpper t
strictFunApply HiFunToLower [HiValueString t] = return $ HiValueString $ T.toLower t
strictFunApply HiFunReverse [HiValueString t] = return $ HiValueString $ T.reverse t
strictFunApply HiFunReverse [HiValueList lst] = return $ HiValueList $ S.reverse lst
strictFunApply HiFunReverse [HiValueBytes b] = return $ HiValueBytes $ B.reverse b
strictFunApply HiFunTrim [HiValueString t] = return $ HiValueString $ T.strip t
strictFunApply HiFunList values = return $ HiValueList $ S.fromList values
strictFunApply HiFunRange [HiValueNumber l, HiValueNumber r] = strictFunApply HiFunList $ HiValueNumber <$> [l .. r]
strictFunApply HiFunFold [HiValueFunction _, HiValueList S.Empty] = return HiValueNull
strictFunApply HiFunFold [HiValueFunction _, HiValueList (x S.:<| S.Empty)] = return x
strictFunApply HiFunFold [HiValueFunction f, HiValueList (a S.:<| b S.:<| rest)] = do
  res <- evaluateLazyFunApply f [HiExprValue a, HiExprValue b]
  strictFunApply HiFunFold [HiValueFunction f, HiValueList (res S.:<| rest)]
strictFunApply HiFunPackBytes [HiValueList lst] = traverse safeToWord8 lst <&> HiValueBytes . B.pack . toList
strictFunApply HiFunUnpackBytes [HiValueBytes b] = strictFunApply HiFunList $ HiValueNumber . toRational <$> B.unpack b
strictFunApply HiFunEncodeUtf8 [HiValueString t] = return $ HiValueBytes $ encodeUtf8 t
strictFunApply HiFunDecodeUtf8 [HiValueBytes b] = return $ either (const HiValueNull) HiValueString (decodeUtf8' b)
strictFunApply HiFunZip [HiValueBytes b] = return $ HiValueBytes $ L.toStrict $ compressWith defaultCompressParams {compressLevel = bestCompression} (L.fromStrict b)
strictFunApply HiFunUnzip [HiValueBytes b] = return $ HiValueBytes $ L.toStrict $ decompressWith defaultDecompressParams (L.fromStrict b)
strictFunApply HiFunSerialise [v] = return $ HiValueBytes $ L.toStrict $ serialise v
strictFunApply HiFunDeserialise [HiValueBytes v] = return $ deserialise $ L.fromStrict v
strictFunApply HiFunRead [HiValueString t] = return $ HiValueAction $ HiActionRead $ T.unpack t
strictFunApply HiFunWrite [HiValueString t, HiValueString d] = return $ HiValueAction $ HiActionWrite (T.unpack t) (encodeUtf8 d)
strictFunApply HiFunWrite [HiValueString t, HiValueBytes b] = return $ HiValueAction $ HiActionWrite (T.unpack t) b
strictFunApply HiFunMkDir [HiValueString t] = return $ HiValueAction $ HiActionMkDir $ T.unpack t
strictFunApply HiFunChDir [HiValueString t] = return $ HiValueAction $ HiActionChDir $ T.unpack t
strictFunApply HiFunParseTime [HiValueString t] = return $ maybe HiValueNull HiValueTime (readMaybe (T.unpack t))
strictFunApply HiFunRand [HiValueNumber l, HiValueNumber r] = do
  lRes <- safeToInt l
  rRes <- safeToInt r
  return $ HiValueAction $ HiActionRand lRes rRes
strictFunApply HiFunEcho [HiValueString t] = return $ HiValueAction $ HiActionEcho t
strictFunApply HiFunKeys [HiValueDict m] = return $ HiValueList $ S.fromList $ Map.keys m
strictFunApply HiFunValues [HiValueDict m] = return $ HiValueList $ S.fromList $ Map.elems m
strictFunApply HiFunCount [HiValueString t] = countToDict (T.unpack t) (HiValueString . T.singleton)
strictFunApply HiFunCount [HiValueBytes b] = countToDict (B.unpack b) (HiValueNumber . toRational)
strictFunApply HiFunCount [HiValueList lst] = countToDict lst id
strictFunApply HiFunInvert [HiValueDict dict] =
  return $
    HiValueDict $
      foldl'
        ( \m tpl ->
            Map.insertWith
              (\(HiValueList n) (HiValueList o) -> HiValueList $ n S.>< o)
              (snd tpl)
              (HiValueList $ S.singleton $ fst tpl)
              m
        )
        Map.empty
        (Map.assocs dict)
strictFunApply _ _ = throwE HiErrorInvalidArgument

evaluateLazyIf :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evaluateLazyIf [cond, a, b] = do
  res <- evaluate cond
  case res of
    HiValueBool True -> evaluate a
    HiValueBool False -> evaluate b
    _ -> throwE HiErrorInvalidArgument
evaluateLazyIf _ = throwE HiErrorInvalidArgument

evaluateLazyAnd :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evaluateLazyAnd [a, b] = do
  aRes <- evaluate a
  case aRes of
    HiValueBool False -> return aRes
    HiValueNull -> return aRes
    _ -> evaluate b
evaluateLazyAnd _ = throwE HiErrorInvalidArgument

evaluateLazyOr :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evaluateLazyOr [a, b] = do
  aRes <- evaluate a
  case aRes of
    HiValueBool False -> evaluate b
    HiValueNull -> evaluate b
    _ -> return aRes
evaluateLazyOr _ = throwE HiErrorInvalidArgument

evaluateLazyFunApply :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evaluateLazyFunApply f args =
  let ar = arity f
   in do
        unless (ar == length args || ar == maxBound) (throwE HiErrorArityMismatch)
        case f of
          HiFunIf -> evaluateLazyIf args
          HiFunAnd -> evaluateLazyAnd args
          HiFunOr -> evaluateLazyOr args
          _ -> traverse evaluate args >>= strictFunApply f

safeGetByIndex ::
  Monad m =>
  (Int -> HiValue) ->
  Rational ->
  Int ->
  ExceptT HiError m HiValue
safeGetByIndex getter r len = do
  idx <- safeToInt r
  return $ if idx >= 0 && idx < len then getter idx else HiValueNull

leftSliceIndex :: Monad m => HiValue -> Int -> ExceptT HiError m Int
leftSliceIndex (HiValueNumber r) len = do
  idx <- safeToInt r
  return $ if idx >= 0 then idx else (idx + len) `mod` len
leftSliceIndex HiValueNull _ = return 0
leftSliceIndex _ _ = throwE HiErrorInvalidArgument

rightSliceIndex :: Monad m => HiValue -> Int -> ExceptT HiError m Int
rightSliceIndex HiValueNull len = return len
rightSliceIndex i len = leftSliceIndex i len

safeSlice ::
  Monad m =>
  HiValue ->
  HiValue ->
  Int ->
  (a -> HiValue) ->
  (Int -> a -> a) ->
  (Int -> a -> a) ->
  a ->
  ExceptT HiError m HiValue
safeSlice l r len cons dropper taker elems = do
  idxL <- leftSliceIndex l len
  idxR <- rightSliceIndex r len
  return $ cons $ dropper idxL $ taker idxR elems

evaluateStringApply :: HiMonad m => T.Text -> [HiValue] -> ExceptT HiError m HiValue
evaluateStringApply t [HiValueNumber r] = safeGetByIndex (HiValueString . T.singleton . T.index t) r (T.length t)
evaluateStringApply t [l, r] = safeSlice l r (T.length t) HiValueString T.drop T.take t
evaluateStringApply _ _ = throwE HiErrorInvalidArgument

evaluateListApply :: HiMonad m => S.Seq HiValue -> [HiValue] -> ExceptT HiError m HiValue
evaluateListApply t [HiValueNumber r] = safeGetByIndex (S.index t) r (S.length t)
evaluateListApply t [l, r] = safeSlice l r (S.length t) HiValueList S.drop S.take t
evaluateListApply _ _ = throwE HiErrorInvalidArgument

evaluateBytesApply :: HiMonad m => B.ByteString -> [HiValue] -> ExceptT HiError m HiValue
evaluateBytesApply t [HiValueNumber r] = safeGetByIndex (HiValueNumber . toRational . B.index t) r (B.length t)
evaluateBytesApply t [l, r] = safeSlice l r (B.length t) HiValueBytes B.drop B.take t
evaluateBytesApply _ _ = throwE HiErrorInvalidArgument

evaluateDictApply :: HiMonad m => Map.Map HiValue HiValue -> [HiValue] -> ExceptT HiError m HiValue
evaluateDictApply d [key] = return $ fromMaybe HiValueNull $ Map.lookup key d
evaluateDictApply _ _ = throwE HiErrorInvalidArgument

evaluateExprApply :: HiMonad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
evaluateExprApply expr args = do
  exprValue <- evaluate expr
  case exprValue of
    HiValueFunction hf -> evaluateLazyFunApply hf args
    HiValueString txt -> traverse evaluate args >>= evaluateStringApply txt
    HiValueList sq -> traverse evaluate args >>= evaluateListApply sq
    HiValueBytes bs -> traverse evaluate args >>= evaluateBytesApply bs
    HiValueDict mp -> traverse evaluate args >>= evaluateDictApply mp
    _ -> throwE HiErrorInvalidFunction

evaluate :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evaluate (HiExprValue value) = return value
evaluate (HiExprApply expr args) = evaluateExprApply expr args
evaluate (HiExprRun expr) = do
  exprValue <- evaluate expr
  case exprValue of
    HiValueAction ha -> lift $ runAction ha
    _ -> throwE HiErrorInvalidArgument
evaluate (HiExprDict pairs) = do
  pairsE <-
    traverse
      ( \(a, b) -> do
          resA <- evaluate a
          resB <- evaluate b
          return (resA, resB)
      )
      pairs
  return $ HiValueDict $ Map.fromList pairsE

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evaluate
