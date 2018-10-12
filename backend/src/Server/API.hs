{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Server.API
  ( app

  -- APIs
  , bazaarAPI
  , bazaarStaticAPI
  , bazaarFullAPI
  )
  where

import           Data.Proxy                       (Proxy (..))
import qualified Data.Text                        as T
import           Data.UUID                        (UUID)

import           Servant                          (serveDirectoryWebApp)
import           Servant.API
import           Servant.Server

import           Servant.API.ContentTypesExtended (HTML, RawHtml)
import           Server.API.Types
import qualified Server.Config
import qualified Server.Handler
import qualified Server.Monad

type BazaarAPI
  = "health" :> Get '[JSON] T.Text
  -- TODO: return PublicItem in /api/v0/search
  :<|> "api" :> "v0" :> "search" :> QueryParam "q" T.Text :> Get '[JSON] [PublicItem]
  :<|> "api" :> "v0" :> "keywords" :> Get '[JSON] [PublicKeyword]
  -- TODO: add a capture here to get the UUID of the Item
  :<|> "api" :> "v0" :> "item-url" :> Capture "uuid" UUID :> Get '[JSON] T.Text

type BazaarStaticAPI
  = Get '[HTML] RawHtml
  :<|> Raw

type BazaarFullAPI
  = BazaarAPI
  :<|> BazaarStaticAPI

bazaarAPI :: Proxy BazaarAPI
bazaarAPI = Proxy

bazaarStaticAPI :: Proxy BazaarStaticAPI
bazaarStaticAPI = Proxy

bazaarFullAPI :: Proxy BazaarFullAPI
bazaarFullAPI = Proxy

serverAPI
  :: (Server.Monad.App :~> Handler)
  -> ServerT BazaarAPI Handler
serverAPI nt = enter nt
  $ Server.Handler.health
  :<|> Server.Handler.search
  :<|> Server.Handler.keywords
  :<|> Server.Handler.itemUrl

files :: ServerT BazaarStaticAPI Handler
files = Server.Handler.root :<|> serveDirectoryWebApp "static"

serverFull
  :: (Server.Monad.App :~> Handler)
  -> ServerT BazaarFullAPI Handler
serverFull nt = serverAPI nt :<|> files

app
  :: Server.Config.Handle
  -> Application
app handle = serve bazaarFullAPI
  $ serverFull
  $ Server.Monad.transformAppToHandler handle
