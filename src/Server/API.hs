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

import           Servant                          (serveDirectoryWebApp)
import           Servant.API
import           Servant.Server

import           Servant.API.ContentTypesExtended (HTML, RawHtml)
import qualified Server.Config
import qualified Server.Handler
import qualified Server.Monad


type BazaarAPI
  = "health" :> Get '[JSON] T.Text
  :<|> "api" :> "v0" :> "search" :> QueryParam "q" T.Text :> Get '[JSON] T.Text

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
serverAPI nt = enter nt $ Server.Handler.health :<|> Server.Handler.search

files :: ServerT BazaarStaticAPI Handler
files = Server.Handler.root :<|> serveDirectoryWebApp "static"

serverFull
  :: (Server.Monad.App :~> Handler)
  -> ServerT BazaarFullAPI Handler
serverFull nt = serverAPI nt :<|> files

app :: Server.Config.Handle -> Application
app handle = serve bazaarFullAPI
  $ serverFull
  $ Server.Monad.transformAppToHandler handle
