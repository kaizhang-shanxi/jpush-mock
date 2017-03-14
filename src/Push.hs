{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Push
  ( RequestBody
  , Response(..)
  ) where

import           Data.Aeson
import           Data.Aeson.TH hiding (Options)

-- TODO: 支持 Android, IOS, WinPhone
data Platform = All deriving (Eq, Show)

instance FromJSON Platform where
  parseJSON (String "all") = return All
  parseJSON _              = fail "platform must be all"

instance ToJSON Platform where
  toJSON All = "all"

data Audience = Audience
  { registration_id :: [String]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Audience)

data CustomContent = CustomContent
  { request_id :: String
  , push_ts    :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''CustomContent)

data Android = Android
  { alert  :: String
  , title  :: String
  , extras :: CustomContent
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Android)

data Notification = Notification
  { android :: Android
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Notification)

data Options = Options
  { time_to_live :: Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Options)

-- RequestBody 表示请求 Body 里应填充的数据
data RequestBody = RequestBody
  { platform     :: Platform
  , audience     :: Audience
  , notification :: Notification
  , options      :: Options
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''RequestBody)

-- Response 表示响应
data Response = Response
  { send_no :: String
  , msg_id  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Response)
