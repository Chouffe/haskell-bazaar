{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Server.API
  ( Config
  , config
  , cPort

  , Port

  , Handle
  , hConfig

  , withServer    -- ^ Running action with server in another thread
  , startServer   -- ^ Async server
  , startServer'  -- ^ Blocking server
  , killServer    -- ^ Kill the thread Id of the server

  , bazaarAPI
  )
  where

import           Control.Concurrent       (ThreadId, forkIO, killThread,
                                           threadDelay)
import           Control.Exception        (bracket)
import           Control.Monad            (unless)
import           Control.Monad.Except     (throwError)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (ask)
import           Control.Monad.State      (get)
import           Control.Monad.Writer     (tell)
import qualified Data.ByteString.Lazy     as BS
import qualified Data.List                as L
import           Data.Proxy               (Proxy (..))
import           Data.Semigroup           ((<>))
import qualified Data.Text                as T
import           GHC.Conc                 (TVar, atomically, newTVarIO,
                                           readTVar, writeTVar)

import           Data.Aeson               (encode)
import           Network.HTTP.Media       ((//), (/:))
import           Network.Wai.Handler.Warp (run)
import           Servant                  (serveDirectoryWebApp)
import           Servant.API
import           Servant.Server

import qualified Database
import qualified Environment
import qualified Logger
import qualified Server.Handler           as ServerHandler
import qualified Server.Monad             as ServerMonad


-- TODO: move to somewhere handy
type Port = Int
data HTML
newtype RawHtml = RawHtml { unRaw :: BS.ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ =  unRaw

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
  :: (ServerMonad.AppM :~> Handler)
  -> ServerT BazaarAPI Handler
serverAPI nt = enter nt $ ServerHandler.health :<|> ServerHandler.search

-- TODO: move to Handlers
root :: Handler RawHtml
root = do
  content <- liftIO $ BS.readFile "static/index.html"
  return $ RawHtml content

serverStatic :: ServerT BazaarStaticAPI Handler
serverStatic = root :<|> serveDirectoryWebApp "static"

serverFull
  :: (ServerMonad.AppM :~> Handler)
  -> ServerT BazaarFullAPI Handler
serverFull nt = serverAPI nt :<|> serverStatic

data Config
  = Config
    { cEnvironment :: Environment.Environment -- ^ Environment
    , cPort        :: Port                    -- ^ Port to run the server on
    }
  deriving (Eq, Show)

config :: Environment.Environment -> Config
config Environment.Test = Config Environment.Test 8003
config Environment.Dev  = Config Environment.Dev 8002
config Environment.Prod = Config Environment.Dev 8001

data Handle
  = Handle
    { hServerThread :: ThreadId         -- ^ Server threadId
    , hDB           :: Database.Handle  -- ^ Database Handle
    , hLogger       :: Logger.Handle    -- ^ Logger Handle
    , hConfig       :: Config           -- ^ Server Config
    }

withServer
  :: Config
  -> Logger.Handle
  -> Database.Handle
  -> (Handle -> IO a)
  -> IO a
withServer config loggerHandle databaseHandle = bracket
  (startServer config loggerHandle databaseHandle)
  killServer

startServer
  :: Config
  -> Logger.Handle
  -> Database.Handle
  -> IO Handle
startServer config@Config {..} loggerHandle databaseHandle = do
  threadId <- forkIO $ startServer' config loggerHandle databaseHandle

  -- Hack :( wait till the server is properly started
  threadDelay 10000

  return $ Handle threadId databaseHandle loggerHandle config

startServer'
  :: Config
  -> Logger.Handle
  -> Database.Handle
  -> IO ()
startServer' config@Config {..} loggerHandle databaseHandle = run cPort
  $ serve bazaarFullAPI
  $ serverFull
  $ ServerMonad.transformAppMToHandlerNT (loggerHandle, databaseHandle)

killServer :: Handle -> IO ()
killServer = killThread . hServerThread
