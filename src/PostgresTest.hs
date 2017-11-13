{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module PostgresTest where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Control.Monad
import Control.Applicative
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Char8 as BS8

data Dictionary = Dictionary {
  word :: Maybe String,
  definition :: Maybe String
} deriving (Show, Generic)

instance ToRow Dictionary where
  toRow d = [toField (word d), toField (definition d)]

instance FromRow Dictionary where
  fromRow = Dictionary <$> field <*> field

instance ToJSON Dictionary

instance FromJSON Dictionary

test = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "test",
    connectUser = "postgres",
    connectPassword = "postgres"
  }

  putStrLn "Enter a new word"
  word <- getLine
  putStrLn "What does it mean?"
  def <- getLine
  execute conn "insert into words (id, word, definition) values (DEFAULT, ?, ?)" $ Dictionary (Just word) (Just def)

  -- mapM_ print =<< ( query_ conn "select word, definition from words" :: IO [Dictionary] )

  mapM_ print =<< ( query_ conn "select word, definition from words" :: IO [Dictionary] )

