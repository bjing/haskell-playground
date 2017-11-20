import CountEntries (listDirectory, MonadFile(..))
import           Test.Hspec

-- step 4, bind MonadFile for testing purpose
-- checkout https://hackage.haskell.org/package/test-fixture for more monadic typeclasses
instance MonadFile Maybe where
  getDirectoryContents = const $ Just ["alien1", "alien2"]
  doesDirectoryExist = const Nothing


main :: IO ()
main = hspec spec

spec = describe "CountEntries" $
  it "should be able listDirectory" $
    -- step 5, test it
    listDirectory "alienPlace" `shouldBe` Just ["alien1", "alien2"]
