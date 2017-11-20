-- file: ch18/CountEntries.hs
module CountEntries (listDirectory, countEntriesTrad, MonadFile(..)) where

-- import System.Directory (doesDirectoryExist, getDirectoryContents)
import           Control.Monad   (forM, liftM)
import           System.FilePath ((</>))

-- step 1. define Monad and abstract methods
class Monad m => MonadFile m where
  getDirectoryContents :: FilePath -> m [String]
  doesDirectoryExist :: FilePath -> m Bool

-- step 2. use MonadFile instead of IO
listDirectory :: (MonadFile m) => FilePath -> m [String]
listDirectory = fmap (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

countEntriesTrad :: (MonadFile m) => FilePath -> m [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
              then countEntriesTrad newName
              else return []
  return $ (path, length contents) : concat rest
