{-# LANGUAGE OverloadedStrings #-}

module MockRequest
    ( makeMockPushyRequest
    ) where

import           PushyClient           (makePushyPostRequest)
import           Types.PushyRequest    (defaultPushyPostRequestBody)
import           Types.PushyResponse   (PushyResult)

import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as D



-- | Sample payload type containing PN message; uses the key 'message'
newtype BodyData = BodyData D.Text deriving (Show)

instance ToJSON BodyData where
    toJSON (BodyData msg) = object ["message" .= toJSON msg]


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
