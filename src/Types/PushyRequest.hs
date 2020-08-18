{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.PushyRequest
    ( BodyData
    , IosNotification (..)
    , defaultIosNotification
    , PushyPostRequestBody (..)
    , defaultPushyPostRequestBody
    ) where

import           Data.Aeson
import           Data.Map

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as L
import qualified Data.Text             as D
import qualified Data.Text             as T


-- | Type to represent the body containing the message
newtype BodyData = BodyData T.Text deriving (Show)

instance ToJSON BodyData where
    toJSON (BodyData msg) = object ["message" .= toJSON msg]

-- | Type to represent the iOS notification settings;
-- see https://pushy.me/docs/api/send-notifications for the complete documentation
data IosNotification = IosNotification
    {
      -- | Main alert message
      inBody         :: T.Text

      -- | Number to display as badge of the app icon
    , inBadge        :: Maybe Int

      -- | Name of soundfile that should be played when a PN is received
    , inSound        :: Maybe T.Text

      -- | String describing pupose of notification
    , inTitle        :: Maybe T.Text

      -- | String used to determine custom notification UI
    , inCategory     :: Maybe T.Text

    -- | Localization key present in app's 'Localizable.strings' file
    , inLocKey       :: Maybe T.Text

    -- | Replacement strings to subsitute in place of the '%@' placeholders of the
    -- localization string
    , inLocArgs      :: [T.Text]

    -- | Localization key present in app's 'Localizable.strings' file
    , inTitleLocKey  :: Maybe T.Text

    -- | Replacement strings to subsitute in place of the '%@' placeholders of the
    -- localization string
    , inTitleLocArgs :: [T.Text]
    } deriving (Show)

instance ToJSON IosNotification where
    toJSON IosNotification{..} =
        object [ "body"           .= inBody
               , "badge"          .= inBadge
               , "sound"          .= inSound
               , "title"          .= inTitle
               , "category"       .= inCategory
               , "loc_key"        .= inLocKey
               , "loc_args"       .= inLocArgs
               , "title_loc_key"  .= inTitleLocKey
               , "title_loc_args" .= inTitleLocArgs
               ]

-- | Default iOS notification setting
defaultIosNotification :: T.Text -> IosNotification
defaultIosNotification body =
    let inBody =  body
        inBadge = Nothing
        inSound = Nothing
        inTitle = Nothing
        inCategory = Nothing
        inLocKey = Nothing
        inLocArgs = []
        inTitleLocKey = Nothing
        inTitleLocArgs = []
    in IosNotification {..}


-- | Type to represent the body of an HTTP POST request to the Pushy API;
--  see https://pushy.me/docs/api/send-notifications for the complete documentation
data PushyPostRequestBody = PushyPostRequestBody
    {
      -- | The unique token associated with the device to which the notification is sent
      pprbTo               :: T.Text

      -- | The payload to be sent to devices
    , pprbBodyData         :: BodyData -- TODO: This should be a Map String String

    -- | How long the push notification should be kept alive
    , pprbTimeToLive       :: Maybe Int

    -- | When set to 'true', invokes app's notification handler even if app is running in
    -- the background
    , pprbContentAvailable :: Maybe Bool

    -- | When set to 'true', the app's notification service extension is invoked even if
    -- the app is running in the background
    , pprbMutableContent   :: Maybe Bool

    -- | Notification options for iOS
    , pprbNotification     :: Maybe IosNotification
    } deriving (Show)

instance ToJSON PushyPostRequestBody where
    toJSON PushyPostRequestBody{..} =
        object [ "to"                .= pprbTo
               , "data"              .= pprbBodyData
               , "time_to_live"      .= pprbTimeToLive
               , "content_available" .= pprbContentAvailable
               , "mutable_content"   .= pprbMutableContent
               , "notification"      .= pprbNotification
               ]

defaultPushyPostRequestBody :: T.Text -- ^ The unique device token must be provided
                            -> BodyData -- ^ The body must be provided
                            -> PushyPostRequestBody
defaultPushyPostRequestBody deviceToken body =
    let pprbTo = deviceToken
        pprbBodyData = body
        pprbTimeToLive = Nothing
        pprbContentAvailable = Nothing
        pprbMutableContent = Nothing
        pprbNotification = Nothing
    in PushyPostRequestBody{..}
