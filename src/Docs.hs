{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
module Docs where

import           Servant
import           Servant.Docs

import qualified Push
import qualified Report

instance ToSample Push.RequestBody where
  toSamples _ =
    [ ("Push request body sample", Push.RequestBody {
          Push.platform = Push.All,
          Push.audience = Push.Audience {
              Push.registration_id = ["1"]
          },
          Push.notification = Push.Notification {
              Push.android = Push.Android {
                  Push.alert = "test",
                  Push.title = "test",
                  Push.extras = Push.CustomContent {
                      Push.request_id = "request_id",
                      Push.push_ts = "1"
                  }
              }
          },
          Push.options = Push.Options {
              Push.time_to_live = 1
          }
      })
    ]

instance ToSample Push.Response where
  toSamples _ =
    [ ("Push response sample", Push.Response "1" "1001")
    ]

instance ToParam (QueryParam "msg_ids" String) where
  toParam _ =
    DocQueryParam "msg_ids"
                  ["1,2,3"]
                  "要查询的消息 ID 列表，以逗号分隔"
                  Normal

instance ToSample [Report.MessageStatus] where
  toSamples _ =
    [ ("Report message status", [ Report.MessageStatus "1" 1 0 0 0
                                , Report.MessageStatus "2" 1 0 0 0
                                , Report.MessageStatus "3" 1 0 0 0
                                ])
    ]
