module DynamoSimpleTest where

data Test = Test {
  category :: T.Text
, user     :: T.Text
, subject  :: T.Text
, replies  :: Int
} deriving (Show)
-- Generate instances and category', user' etc. variables for queries/updates
mkTableDefs "migrate" (tableConfig "" (''Test, WithRange) [] [])

test :: IO ()
test = do
  lgr <- newLogger Info stdout
  setEnv "AWS_ACCESS_KEY_ID" "XXXXXXXXXXXXXX"
  setEnv "AWS_SECRET_ACCESS_KEY" "XXXXXXXXXXXXXXfdjdsfjdsfjdskldfs+kl"
  env  <- newEnv Discover
  let dynamo = setEndpoint False "localhost" 8000 dynamoDB
  let newenv = env & configure dynamo & set envLogger lgr
  runResourceT $ runAWS newenv $ do
      migrate mempty Nothing -- Create tables, indices etc.
      --
      putItem (Test "news" "john" "test" 20)
      --
      item <- getItem Eventually tTest ("news", "john")
      liftIO $ print item
      --
      items <- scanCond tTest (replies' >. 15) 10
      liftIO $ print items