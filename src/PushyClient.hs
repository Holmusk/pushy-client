-- | Types to capture the request schema for Pushy's external push notification
-- API; see https://pushy.me/docs/api/send-notifications for more information

{-# LANGUAGE OverloadedStrings   #-}
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
import qualified Data.Text             as D


-- | Function to contruct the Pushy HTTP POST request
constructPushyPostRequest :: B.ByteString -- ^ API key
                          -> PushyPostRequestBody -- ^ The body of the post request
                          -> Request
constructPushyPostRequest apiKey pprBody = do
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
    pushyRequestBody = RequestBodyLBS $ encode $ pprBody

-- | Function to ping the Pushy API endpoint
makePushyPostRequest :: B.ByteString -- ^ API key
                     -> PushyPostRequestBody -- ^ The body of the post request
                     -> IO PushyResult
makePushyPostRequest apiKey pprBody  =
    handle (\ (he :: HttpException) -> pure $ PushyResultHttpLibraryError he "Http lib error") $ do
        let hReq = constructPushyPostRequest apiKey pprBody
        hRes <- httpLBS hReq
        pure $ decodePushyResponse hRes

-- | Utility to make pushy post requests with a secret key and a device token for some
-- a pushy account. This only requires the API key, the device token, and the message to
-- be specified; the function then uses the default 'PushyPostRequestBody' value to construct
-- the request and ping the Pushy external API endpoint. Run this function on the REPL.
makeMockPushyRequest :: String -- ^ API key
                     -> String -- ^ Receiever token
                     -> String -- ^ Message to be sent
                     -> IO PushyResult
makeMockPushyRequest apiKey deviceToken msg =
    let byteStringApiKey = B8.pack apiKey
        textDeviceToken = D.pack deviceToken
        textMsg = D.pack msg
        pprBody = defaultPushyPostRequestBody textDeviceToken $ BodyData textMsg
    in makePushyPostRequest byteStringApiKey pprBody
