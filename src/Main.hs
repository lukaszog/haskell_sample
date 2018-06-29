{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.IO.Class               (liftIO)

import           Network.HTTP.Types
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Scotty
import           Control.Monad.Logger    (runStderrLoggingT)

import qualified Database.Persist.Postgresql          as Db

import           Models
import           ModelsJson
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger

main :: IO()
--main :: Db.SqlPersist (ResourceT (NoLoggingT IO)) a -> IO a
main = runStderrLoggingT $ Db.withPostgresqlPool connStr 10 $ \pool -> do

  -- Migrate database
  runDB pool $ Db.runMigration migrateAll

  -- Start server
  scotty 5001 $ do

    middleware logStdoutDev

    -- * Static content middleware
    --middleware $ staticPolicy (noDots >-> addBase "public")

    get "/" $
      html $ "<h1>Backend API server (haskell Scotty)</h1>"

    -- * CRUD

    get "/api/users" $ do
      (users :: [Db.Entity User]) <-
        liftIO $ runDB pool $ Db.selectList [] []
      json users

    post "/api/users" $ do
      (user :: User) <- jsonData
      uid <- liftIO $ runDB pool $ Db.insert user
      json $ Db.Entity uid user


connStr :: Db.ConnectionString
connStr =  "host=localhost dbname=road_free_development user=cidevant password='' port=5432"

runDB :: Db.ConnectionPool ->  Db.SqlPersistM a -> IO a
runDB = flip Db.runSqlPersistMPool
