{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Types.PushyResponse
    ( SuccessInfo (..)
    , FailureInfo (..)
    , PushyResult (..)
    , decodePushyResponse
    ) where

import           Data.Aeson
import           Data.Map
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.HTTP.Types

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as L
import qualified Data.Text             as D
import qualified Data.Text             as T


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
      prfPushyErrorCode        :: T.Text

      -- | A detailed description of the exact error; also provided by the Pushy API
    , prfPushyErrorDescription :: T.Text
    } deriving (Show)

instance FromJSON FailureInfo where
    parseJSON = withObject "FailureInfo" $ \v -> FailureInfo
        <$> v .: "code"
        <*> v .: "error"

-- | Sum type to capture possible results of the pushy push notification
data PushyResult =
  -- | Represents HTTP responses with a 200 status code; wraps the unique ID of the PN
    PushyResultSuccesful Status SuccessInfo

    -- | Represents HTTP response with a 300 / 400 / 500 status code; wraps
    -- the HTTP status code and the failure info of type 'FailureInfo'
    | PushyResultHttpFailure Status FailureInfo

    -- | Represents other possible failures
    | PushyResultOtherFailure T.Text
    deriving (Show)

-- | Function to decode the Pushy API HTTP response
decodePushyResponse :: Response L.ByteString -> PushyResult
decodePushyResponse res =
    let resStat = responseStatus res
        resBody = responseBody res
    in if statusCode resStat == 200
       then case decode @SuccessInfo resBody of
                Just info -> PushyResultSuccesful resStat info
                Nothing   -> PushyResultOtherFailure "Failed to parse response"
       else case decode @FailureInfo resBody of
                Just info -> PushyResultHttpFailure resStat info
                Nothing   -> PushyResultOtherFailure "Failed to parse response"
