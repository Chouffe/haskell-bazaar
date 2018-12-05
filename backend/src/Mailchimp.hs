{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mailchimp
  ( Config
  , config

  , Handle
  , withHandle

  , subscribe
  , StatusMessage
  )
  where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map               as M
import           Data.Semigroup         ((<>))
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T


import           Control.Lens
import           Data.Aeson             (toJSON)
import           Network.Connection      (TLSSettings (..))
import           Network.Wreq
import           Network.HTTP.Client.TLS (mkManagerSettings)

import qualified Logger

data Config
  = Config
    { cAPIKey  :: !T.Text
    , cListId  :: !String
    , cBaseUrl :: !String
    }
  deriving Show

-- TODO: read from Env and not hardcoded!
config :: Config
config = Config
  { cAPIKey  = "e754bfd555ef7e1256f9e9d5ad009120-us18"
  , cListId  = "e9a2d39f6d"
  , cBaseUrl = "https://us18.api.mailchimp.com/3.0"
  }

data Handle
  = Handle
    { hLogger :: !Logger.Handle
    , hConfig :: !Config
    }

type StatusMessage = T.Text

subscribe
  :: MonadIO m
  => Handle
  -> T.Text
  -> m (Either StatusMessage StatusMessage)
subscribe Handle{..} emailAddress = do

  liftIO
    $ Logger.info hLogger
    $ "Mailchimp Subscribing " <> show emailAddress

  -- This can throw some exceptions
  -- TODO: better Error Handling
  r  <- liftIO
          $ postWith opts (cBaseUrl <> "/lists/" <> cListId <> "/members")
          $ toJSON
          $ M.fromList
            [ ("email_address" :: T.Text, emailAddress :: T.Text)
            , ("status", "subscribed")
            ]

  let sc = r ^. responseStatus . statusCode
  let sm = T.decodeUtf8 (r ^. responseStatus . statusMessage)

  return $
    if sc == 200
       then Right sm
       else Left sm

  where
    Config{..} = hConfig
    opts :: Options
    opts = defaults
             & auth ?~ basicAuth "anystring" (T.encodeUtf8 cAPIKey)
             & header "Accept" .~ ["application/json"]
             & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)

withHandle :: Logger.Handle -> Config -> (Handle -> IO a) -> IO a
withHandle loggerHandle cfg f =
  let handle = Handle { hConfig = cfg, hLogger = loggerHandle }
  in f handle
