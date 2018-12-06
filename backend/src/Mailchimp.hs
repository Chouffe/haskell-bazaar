{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mailchimp
  ( Config
  , getConfig

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

getConfig :: IO Config
getConfig = do
  apiKey  <- (T.pack . cleanString) <$> readFile "/run/secrets/mailchimp-api-key"
  listId  <- cleanString <$> readFile "/run/secrets/mailchimp-list-id"
  baseUrl <- cleanString <$> readFile "/run/secrets/mailchimp-base-url"
  return $ Config apiKey listId baseUrl
  where
    cleanString :: String -> String
    cleanString = filter (\c -> c /= '\n')

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
