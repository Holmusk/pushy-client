-- | Types to capture the request schema for Pushy's external push notification
-- API; see https://pushy.me/docs/api/send-notifications for more information

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PushyClient
    ( makePushyPostRequest
    ) where


import           Types.PushyRequest    (PushyPostRequestBody (..))
import           Types.PushyResponse   (PushyResult (..),
                                        decodePushyResponse)

import           Control.Monad.Catch
import           Data.Aeson
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.HTTP.Types

import qualified Data.ByteString       as B


-- | Function to contruct the Pushy HTTP POST request
constructPushyPostRequest :: (ToJSON payload)
                          => B.ByteString -- ^ API key
                          -> PushyPostRequestBody payload -- ^ The body of the post request
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
makePushyPostRequest :: (ToJSON payload)
                     => B.ByteString -- ^ API key
                     -> PushyPostRequestBody payload -- ^ The body of the post request
                     -> IO PushyResult
makePushyPostRequest apiKey pprBody  =
    handle (\ (he :: HttpException) -> pure $ PushyResultHttpLibraryError he "Http lib error") $ do
        let hReq = constructPushyPostRequest apiKey pprBody
        hRes <- httpLBS hReq
        pure $ decodePushyResponse hRes
