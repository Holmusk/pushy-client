{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PushyClient.Types.PushyRequest
    ( IosNotification (..)
    , defaultIosNotification
    , PushyPostRequestBody (..)
    , defaultPushyPostRequestBody
    ) where

import           Data.Aeson

import qualified Data.Text  as T


-- | Type to represent the iOS notification settings;
-- see https://pushy.me/docs/api/send-notifications for the complete documentation
data IosNotification = IosNotification
    {
      -- | Main alert message
      inBody         :: T.Text

      -- | Number to display as badge of the app icon
    , inBadge        :: Int

      -- | Name of soundfile that should be played when a PN is received
    , inSound        :: T.Text

      -- | String describing pupose of notification
    , inTitle        :: T.Text

      -- | String used to determine custom notification UI
    , inCategory     :: T.Text

    -- | Localization key present in app's 'Localizable.strings' file
    , inLocKey       :: T.Text

    -- | Replacement strings to subsitute in place of the '%@' placeholders of the
    -- localization string
    , inLocArgs      :: [T.Text]

    -- | Localization key present in app's 'Localizable.strings' file
    , inTitleLocKey  :: T.Text

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

-- | Default iOS notification setting. Use this to build a custom 'IosNotification' value.
defaultIosNotification :: IosNotification
defaultIosNotification  =
    let inBody = ""
        inBadge = 1
        inSound = "ping.aiff"
        inTitle = ""
        inCategory = ""
        inLocKey = ""
        inLocArgs = []
        inTitleLocKey = ""
        inTitleLocArgs = []
    in IosNotification {..}


-- | Type to represent the body of an HTTP POST request to the Pushy API. Note that this
-- type is parameterized by the type of the payload, since Pushy supports arbitrary JSON
-- objects as JSON bodies. The type of the payload should have a 'ToJSON' instance.
-- See https://pushy.me/docs/api/send-notifications for the complete documentation
data PushyPostRequestBody payload = PushyPostRequestBody
    {
      -- | The unique token associated with the device to which the notification is sent
      pprbTo               :: T.Text

      -- | The payload to be sent to devices
    , pprbBodyData         :: payload

    -- | How long the push notification should be kept alive; the default is set to a month
    -- with 30 days
    , pprbTimeToLive       :: Int

    -- | When set to 'true', invokes app's notification handler even if app is running in
    -- the background; default is 'False'
    , pprbContentAvailable :: Bool

    -- | When set to 'true', the app's notification service extension is invoked even if
    -- the app is running in the background; default is 'False'
    , pprbMutableContent   :: Bool

    -- | Notification options for iOS
    , pprbIosNotification  :: Maybe IosNotification
    } deriving (Show)

instance (ToJSON payload) => ToJSON (PushyPostRequestBody payload) where
    toJSON PushyPostRequestBody{..} =
        object [ "to"                .= pprbTo
               , "data"              .= toJSON pprbBodyData
               , "time_to_live"      .= pprbTimeToLive
               , "content_available" .= pprbContentAvailable
               , "mutable_content"   .= pprbMutableContent
               , "notification"      .= pprbIosNotification
               ]

-- | The default value for a pushy post request body. Use this to construct custom
-- Pushy post request bodies. Note that the @pprbNotification@ field must be configured
-- if your application is run on iOS devices.
defaultPushyPostRequestBody :: T.Text -- ^ The unique device token must be provided
                            -> payload -- ^ The payload of arbitrary type
                            -> PushyPostRequestBody payload
defaultPushyPostRequestBody deviceToken body =
    PushyPostRequestBody
        { pprbTo = deviceToken
        , pprbBodyData = body
        , pprbTimeToLive = 2592000
        , pprbContentAvailable = False
        , pprbMutableContent = False
        , pprbIosNotification = Just defaultIosNotification
        }
