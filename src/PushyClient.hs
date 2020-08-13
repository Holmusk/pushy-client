{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PushyClient
    ( foo
    , makePushyPostRequest
    ) where

import           Control.Monad.Catch
import           Data.Aeson
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as L
import qualified Data.Text             as D
import qualified Data.Text             as T
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.HTTP.Types

-- | Type to represent the body containing the message
newtype BodyData = BodyData T.Text deriving (Show)

instance ToJSON BodyData where
    toJSON (BodyData msg) = object ["message" .= msg]

-- | Type to represent the iOS notification settings
data IosNotification = IosNotification
    { inBody  :: T.Text
--    , inBadge :: Int
    , inSound :: T.Text
    } deriving (Show)

-- | Default iOS notification setting
defaultIosNotificationSettings :: IosNotification
defaultIosNotificationSettings =
    let inBody =  "Hello"
--        inBadge = 1
        inSound = T.pack "ping.aiff"
    in IosNotification {..}

instance ToJSON IosNotification where
    toJSON IosNotification {..} =
        object [ "body"  .= inBody
  --             , "badge" .= inBadge
               , "sound" .= inSound
               ]

-- | Type to represent the body of an HTTP POST request to the Pushy API
data PushyPostRequestBody = PushyPostRequestBody
    { to           :: T.Text
    , bodyData     :: BodyData
    , notification :: IosNotification
    } deriving (Show)

instance ToJSON PushyPostRequestBody where
    toJSON (PushyPostRequestBody to bodyData notification) =
        object [ "to" .= to
               , "body" .= toJSON bodyData
               , "notification" .= toJSON notification
               ]

-- | Function to construct the Pushy HTTP POST request body, given the recipient ID and the
-- PN message
constructPushyPostRequestBody :: T.Text -- ^ recipient ID
                              -> T.Text -- ^ PN message
                              -> PushyPostRequestBody
constructPushyPostRequestBody recId msg =
    let to = recId
        bodyData = BodyData msg
        notification = defaultIosNotificationSettings
    in PushyPostRequestBody{..}

-- | Function to contruct the Pushy HTTP POST request
constructPushyPostRequest :: B.ByteString -- ^ API key
                          -> T.Text -- ^ recipient ID
                          -> T.Text -- ^ message to be posted
                          -> Request
constructPushyPostRequest apiKey recId msg = do
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
    pushyRequestBody = RequestBodyLBS $ encode $ constructPushyPostRequestBody recId msg

-- | Datatype to represent possible results of pushy request
data PushyResult =  SuccessfulRequest
                 | HttpLibError
                 | FailureStatusCode Request Int
                 deriving (Show)


-- | Datatype to represent possible error codes:
-- data ErrorCode =  ErrorCode300
--                | ErrorCode400
--                | Other

-- | Function to ping the Pushy API endpoint
makePushyPostRequest :: B.ByteString -- ^ API key
                     -> T.Text -- ^ recipient ID
                     -> T.Text -- ^ message to be posted
                     -> IO PushyResult
makePushyPostRequest apiKey recId msg =
    handle (\ (he :: HttpException) -> pure HttpLibError) $
    let hReq = constructPushyPostRequest apiKey recId msg
    in do
        hRes <- httpLBS hReq
        pure $ decodeRes hReq (responseStatus hRes)
  where
    decodeRes :: Request -> Status -> PushyResult
    decodeRes r s =
        if  200 <= statusCode s && statusCode s < 300
        then SuccessfulRequest
        else FailureStatusCode r (statusCode s)

foo :: String -> String -> String -> IO PushyResult
foo apiKey recId msg =
    let byteStringApiKey = B8.pack apiKey
        textRecId = D.pack recId
        textMsg = D.pack msg
    in makePushyPostRequest byteStringApiKey textRecId textMsg
