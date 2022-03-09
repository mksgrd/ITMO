module Main where

import Control.Monad.Trans.Class (lift)
import Data.Set (fromList)
import HW3.Action (HIO (runHIO), HiPermission (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyError, prettyValue)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> case parse input of
          Left peb -> outputStrLn (errorBundlePretty peb) >> loop
          Right expr -> do
            result <- lift $ runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime])
            outputStrLn (renderString $ layoutPretty defaultLayoutOptions $ either prettyError prettyValue result) >> loop
