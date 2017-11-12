module GetPassphraseMaybeT where

import Control.Monad.Trans

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