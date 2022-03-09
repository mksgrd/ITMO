{-# LANGUAGE DerivingVia #-}

module HW3.Action (HiPermission (..), PermissionException (..), HIO (..)) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import Data.Set (Set, difference, foldl', fromList)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)
import HW3.Base (HiAction (..), HiMonad, HiValue (..), runAction)
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random (newStdGen, uniformR)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

newtype PermissionException = PermissionRequired HiPermission deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}
  deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)

requirePerms :: [HiPermission] -> IO a -> HIO a
requirePerms perms actions =
  HIO $
    foldl'
      (\_ e -> throwIO (PermissionRequired e))
      actions
      . difference (fromList perms)

instance HiMonad HIO where
  runAction (HiActionRead fp) =
    requirePerms
      [AllowRead]
      ( do
          fileExists <- doesFileExist fp
          if fileExists
            then do
              bytes <- B.readFile fp
              return $ either (const $ HiValueBytes bytes) HiValueString (decodeUtf8' bytes)
            else do
              entries <- listDirectory fp
              return $ HiValueList $ S.fromList (HiValueString . T.pack <$> entries)
      )
  runAction (HiActionWrite fp bs) = requirePerms [AllowWrite] (B.writeFile fp bs >> return HiValueNull)
  runAction (HiActionMkDir fp) = requirePerms [AllowWrite] (createDirectory fp >> return HiValueNull)
  runAction (HiActionChDir fp) = requirePerms [AllowRead] (setCurrentDirectory fp >> return HiValueNull)
  runAction HiActionCwd = requirePerms [AllowRead] (HiValueString . T.pack <$> getCurrentDirectory)
  runAction HiActionNow = requirePerms [AllowTime] (HiValueTime <$> getCurrentTime)
  runAction (HiActionRand l r) = requirePerms [] (HiValueNumber . toRational . fst . uniformR (l, r) <$> newStdGen)
  runAction (HiActionEcho t) = requirePerms [AllowWrite] (putStrLn (T.unpack t) >> return HiValueNull)
