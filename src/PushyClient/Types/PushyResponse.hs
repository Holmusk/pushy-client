{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module PushyClient.Types.PushyResponse
    ( SuccessInfo (..)
    , FailureInfo (..)
    , PushyResult (..)
    , decodePushyResponse
    ) where

import           Data.Aeson
import           Network.HTTP.Client
import           Network.HTTP.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.Text            as T


-- | Newtype wrapping the ID of a PN that was succesfully sent by Pushy
newtype SuccessInfo = SuccessInfo T.Text deriving (Show)

instance FromJSON SuccessInfo where
    parseJSON = withObject "SuccessInfo" $ \v -> SuccessInfo <$> v .: "id"

-- | Record type to capture releveant information of a failure HTTP request to the Pushy
-- API
data FailureInfo = FailureInfo
    {
      -- | Specific error codes provided by Pushy;
      -- see the documentation for more info: https://pushy.me/docs/api/send-notifications
      fiPushyErrorCode        :: T.Text

      -- | A detailed description of the exact error; also provided by the Pushy API
    , fiPushyErrorDescription :: T.Text
    } deriving (Show)

instance FromJSON FailureInfo where
    parseJSON = withObject "FailureInfo" $ \v -> FailureInfo
        <$> v .: "code"
        <*> v .: "error"

-- | Sum type to capture possible results of the pushy push notification
data PushyResult =
  -- | Represents HTTP responses with a 200 status code; constructor takes the HTTP status
  -- code and the unique ID of the PN as arguments
    PushyResultSuccessfulRequest Status SuccessInfo

    -- | Represents HTTP response with a 3xx / 4xx / 5xx status code; constructor takes
    -- the HTTP status code and the failure info of type 'FailureInfo' as arguments
    | PushyResultFailedRequest Status FailureInfo

    -- | Represents failure to decode response body; constructor takes the HTTP status code
    -- and the raw response body as arguments
    | PushyResultResBodyFailedDecode Status L.ByteString

    -- | Represents an Network.HTTP error; constructor takes the particular 'HttpExcpetion'
    -- and a 'Text' message as argument
    | PushyResultHttpLibraryError HttpException T.Text
    deriving (Show)

-- | Function to decode the Pushy API HTTP response
decodePushyResponse :: Response L.ByteString -> PushyResult
decodePushyResponse res =
    let resStat = responseStatus res
        resBody = responseBody res
    in if statusCode resStat == 200
       then case decode @SuccessInfo resBody of
                Just info -> PushyResultSuccessfulRequest resStat info
                Nothing   -> PushyResultResBodyFailedDecode resStat resBody
       else case decode @FailureInfo resBody of
                Just info -> PushyResultFailedRequest resStat info
                Nothing   -> PushyResultResBodyFailedDecode resStat resBody
