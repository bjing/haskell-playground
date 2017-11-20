module Main where

import           CountEntries     (MonadFile (..), listDirectory)
import           Lib
import qualified System.Directory as Real (doesDirectoryExist,
                                           getDirectoryContents)

-- step 3. bind it to real implementation
instance MonadFile IO where
  getDirectoryContents = Real.getDirectoryContents
  doesDirectoryExist = Real.doesDirectoryExist

main :: IO ()
main = do
  arr <- listDirectory  "."
  print arr
