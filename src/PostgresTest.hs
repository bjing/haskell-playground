{-# LANGUAGE OverloadedStrings #-}
module PostgresTest where

import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative

data Dictionary = Dictionary {
  word :: Maybe String,
  definition :: Maybe String
} deriving (Show)

instance ToRow Dictionary where
  toRow d = [toField (word d), toField (definition d)]

instance FromRow Dictionary where
  fromRow = Dictionary <$> field <*> field

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
  execute conn "insert into words (word, definition) values (?, ?)" $ Dictionary (Just word) (Just def)

