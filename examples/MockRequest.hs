{-# LANGUAGE OverloadedStrings #-}

module Main where

import           PushyClient                     (makePushyPostRequest)
import           PushyClient.Types.PushyRequest  (IosNotification (..),
                                                  PushyPostRequestBody (..),
                                                  defaultIosNotification,
                                                  defaultPushyPostRequestBody)
import           PushyClient.Types.PushyResponse (PushyResult)

import           Data.Aeson
import qualified Data.ByteString.Char8           as B8
import qualified Data.Text                       as D



-- | Sample payload type containing PN message; uses the key 'message'
newtype BodyData = BodyData D.Text deriving (Show)

instance ToJSON BodyData where
    toJSON (BodyData msg) = object ["message" .= toJSON msg]


-- | Utility to make pushy post requests with a secret key and a device token for some
-- a pushy account. This only requires the API key, the device token, and the message to
-- be specified; the function then uses the default 'PushyPostRequestBody' value to construct
-- the request and ping the Pushy external API endpoint. Note that for iOS, the message must
-- be passed as part of the @pprbIosNotification@ field.
makeMockPushyRequest :: String -- ^ API key
                     -> String -- ^ Receiever token
                     -> String -- ^ Message to be sent
                     -> IO PushyResult
makeMockPushyRequest apiKey deviceToken msg =
    let byteStringApiKey = B8.pack apiKey
        textDeviceToken = D.pack deviceToken
        textMsg = D.pack msg
        iosNotifConfig = defaultIosNotification { inBody  = textMsg
                                                , inTitle = "iOS notification check"
                                                }
        pprBody = defaultPushyPostRequestBody textDeviceToken $ BodyData textMsg
        pprBodyToSend = pprBody { pprbIosNotification = Just iosNotifConfig }
    in makePushyPostRequest byteStringApiKey pprBodyToSend

main :: IO ()
main = do
    putStrLn "Enter Pushy API key: "
    apiKey <- getLine
    putStrLn "Enter device token: "
    deviceToken <- getLine
    putStrLn "Enter message to send: "
    msg <- getLine
    pushyResult <-  makeMockPushyRequest apiKey deviceToken msg
    print pushyResult
