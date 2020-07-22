{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Logger   (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Sqlite
    ( ConnectionPool, createSqlitePool, runSqlPool
    , runSqlPersistMPool, runMigration, selectList
    , delete, insert , entityVal, fromSqlKey, toSqlKey
    , (==.))
import           Data.String.Conversions (cs)
import           Network.Wai
import           Network.Wai.MakeAssets
import           Servant

import           Api
import           Models

type WithAssets = Api :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

options :: Options
options = Options "client"

app :: String -> IO Application
app connStr = serve withAssets <$> server connStr

server :: String -> IO (Server WithAssets)
server connStr = do
  assets <- serveAssets options
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs connStr) 5
  -- run migration
  runSqlPool (runMigration migrateAll) pool
  return (apiServer pool :<|> Tagged assets)

apiServer :: ConnectionPool -> Server Api
apiServer pool = listItems pool :<|> getItem pool :<|> postItem pool :<|> deleteItem pool

listItems :: ConnectionPool -> Handler [Int]
listItems db = liftIO $ allItemIds db

getItem :: ConnectionPool -> Int -> Handler Item
getItem db n = maybe (throwError err404) return =<< liftIO (lookupItem db n)

postItem :: ConnectionPool -> Item -> Handler Int
postItem pool new = liftIO $ insertItem pool new


insertItem :: ConnectionPool -> Item -> IO Int
insertItem pool new = liftIO $ flip runSqlPersistMPool pool $ do
  newKey <- insert new
  return $ fromIntegral $ fromSqlKey newKey

lookupItem :: ConnectionPool -> Int -> IO (Maybe Item)
lookupItem pool id = liftIO $ flip runSqlPersistMPool pool $ do
  items <- selectList [ItemId ==. (toSqlKey $ fromIntegral id)] []
  return $ case head items of
    Entity _ item -> Just item
    _         -> Nothing

allItemIds :: ConnectionPool -> IO [Int]
allItemIds pool = liftIO $ flip runSqlPersistMPool pool $ do
  items <- selectList [] []
  return $ map (\(Entity id (_)) -> fromIntegral $ fromSqlKey (id::ItemId)) items

deleteItem :: ConnectionPool -> Int -> Handler ()
deleteItem pool itemId =  liftIO $ flip runSqlPersistMPool pool $ delete (toSqlKey $ fromIntegral itemId :: (Key Item))
