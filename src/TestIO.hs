{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (readFile)
import qualified Prelude
import Control.Monad.State

class Monad m => FSMonad m where
    readFile :: FilePath -> m String

-- | 4) Counts the number of characters in a file
numCharactersInFile :: FSMonad m => FilePath -> m Int
numCharactersInFile fileName = do
    contents <- readFile fileName
    return (length contents)

instance FSMonad IO where
    readFile = Prelude.readFile

data MockFS = SingleFile FilePath String

instance FSMonad (State MockFS) where
               -- ^ Reader would be enough in this particular case though
    readFile pathRequested = do
        (SingleFile pathExisting contents) <- get
        if pathExisting == pathRequested
            then return contents
            else fail "file not found"


testNumCharactersInFile :: Bool
testNumCharactersInFile = evalState
                                (numCharactersInFile "test.txt")
                                (SingleFile "test.txt" "hello world")
                             == 11


main = do
  print =<< numCharactersInFile "qwe.hs"
  print testNumCharactersInFile