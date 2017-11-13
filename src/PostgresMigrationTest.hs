{-# LANGUAGE OverloadedStrings #-}

module PostgresMigrationTest where

import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BS8

migrate :: IO (MigrationResult String)
migrate = do
  let url = "host=localhost dbname=test user=postgres password=postgres"
  let dir = "resources/"
  con <- connectPostgreSQL (BS8.pack url)
  withTransaction con $ runMigration $
      MigrationContext (MigrationDirectory dir) True con

main :: IO ()
main = undefined
