{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Lib
    ( startApp
    ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy        (ByteString)
import           Data.List.Split
import           Data.Text.Lazy              (pack)
import           Data.Text.Lazy.Encoding     (encodeUtf8)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified Servant.Docs                as Docs
import           System.Random

import           Docs
import qualified Push
import qualified Report

type API = "v3" :> "push" :> ReqBody '[JSON] Push.RequestBody :> Post '[JSON] Push.Response
  :<|> "v3" :> "received" :> QueryParam "msg_ids" String :> Get '[JSON] [Report.MessageStatus]
  :<|> Raw

newtype CounterVal = CounterVal
  { getCounterVal :: Int
  } deriving (Show, Num, FromJSON, ToJSON)

startApp :: IO ()
startApp = do
  initCounter <- newTVarIO (CounterVal 0)
  run 1234 (app initCounter)

app :: TVar CounterVal -> Application
app initCounter = serve api (server initCounter)

api :: Proxy API
api = Proxy

server :: TVar CounterVal -> Server API
server initCounter = sendPush initCounter
  :<|> getReport initCounter
  :<|> serveDocs initCounter
  where
    sendPush counter _ = liftIO $ do
      atomically $ modifyTVar counter (+ 1)
      count <- readTVarIO counter
      messageID <- randomRIO (1000000000, 2000000000) :: IO Int
      return $ Push.Response (show $ getCounterVal count) (show messageID)
    getReport _ messageIDs = case messageIDs of
      Nothing  -> throwError $ err400 { errBody = "msg_ids is required" }
      Just ids -> return $ fakeReport ids

    serveDocs _ _ respond =
      respond $ responseLBS ok200 [plain] docsBS
    plain = ("Content-Type", "text/plain")

fakeReport :: String -> [Report.MessageStatus]
fakeReport messageIDs = map (\id -> Report.MessageStatus id 1 0 0 0) ids
  where
    ids = splitOn "," messageIDs

docsBS :: ByteString
docsBS = encodeUtf8 . pack . Docs.markdown $ Docs.docs api
