-- | Types to capture the request schema for Pushy's external push notification
-- API; see https://pushy.me/docs/api/send-notifications for more information

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PushyClient
    ( makePushyPostRequest
    , makeMockPushyRequest
    ) where


import           Types.PushyRequest    (BodyData (..), IosNotification (..),
                                        PushyPostRequestBody (..),
                                        defaultIosNotification,
                                        defaultPushyPostRequestBody)
import           Types.PushyResponse   (FailureInfo (..), PushyResult (..),
                                        SuccessInfo (..), decodePushyResponse)

import           Control.Monad.Catch
import           Data.Aeson
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.HTTP.Types

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as L
import qualified Data.Text             as D
import qualified Data.Text             as T


-- | Function to contruct the Pushy HTTP POST request
constructPushyPostRequest :: B.ByteString -- ^ API key
                          -> T.Text -- ^ recipient ID
                          -> T.Text -- ^ message to be posted
                          -> Request
constructPushyPostRequest apiKey deviceToken msg = do
    let initReq = parseRequest_ pushyApiPath
      in initReq
         { method = "POST"
         , requestHeaders = [(hContentType, "application/json")]
         , queryString = pushyQueryString
         , requestBody = pushyRequestBody
         }
  where
    pushyApiPath :: String
    pushyApiPath = "https://api.pushy.me/push"

    pushyQueryString :: B.ByteString
    pushyQueryString = "api_key=" <> apiKey

    pushyRequestBody :: RequestBody
    pushyRequestBody = RequestBodyLBS $
                       encode $
                       defaultPushyPostRequestBody deviceToken (BodyData msg)

-- | Function to ping the Pushy API endpoint
makePushyPostRequest :: B.ByteString -- ^ API key
                     -> T.Text -- ^ Device token
                     -> T.Text -- ^ Message
                     -> IO PushyResult
makePushyPostRequest apiKey deviceToken msg  =
    handle (\ (he :: HttpException) -> pure $ PushyResultOtherFailure "Http lib error") $
    let hReq = constructPushyPostRequest apiKey deviceToken msg
    in do
        hRes <- httpLBS hReq
        pure $ decodePushyResponse hRes
  where

makeMockPushyRequest :: String -> String -> String -> IO PushyResult
makeMockPushyRequest apiKey recId msg =
    let byteStringApiKey = B8.pack apiKey
        textRecId = D.pack recId
        textMsg = D.pack msg
    in makePushyPostRequest byteStringApiKey textRecId textMsg
