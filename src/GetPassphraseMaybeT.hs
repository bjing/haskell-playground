module GetPassphraseMaybeT where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad
import Data.Char


getPassphrase :: MaybeT IO String
getPassphrase = do
  s <- lift getLine
  guard (isValid s)
  return s

askPassphrase :: MaybeT IO ()
askPassphrase = do
  lift $ putStrLn "Insert your new passphrase: "
  value <- getPassphrase
  lift $ putStrLn "Storing in database..."

  -- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

main :: IO ()
main = do
  s <- runMaybeT getPassphrase
  case s of
    Just v -> putStrLn v
    Nothing -> putStrLn "No value"

