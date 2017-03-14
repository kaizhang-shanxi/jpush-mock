{-# LANGUAGE TemplateHaskell #-}
module Report
  ( MessageStatus(..)
  ) where

import Data.Aeson.TH

data MessageStatus = MessageStatus
  { msg_id :: String
  , android_received :: Int
  , ios_apns_sent :: Int
  , ios_msg_received :: Int
  , wp_mpns_sent :: Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''MessageStatus)
