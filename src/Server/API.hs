{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server.API
  ( Config
  , config
  , cPort

  , Handle
  , withServer
  , startServer
  , killServer

  , Port
  )
  where

import           Control.Concurrent       (ThreadId, forkIO, killThread,
                                           threadDelay)
import           Control.Exception        (bracket)
import           Control.Monad            (unless)
import           Control.Monad.Except     (throwError)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.State      (get)
import           Control.Monad.Writer     (tell)
import qualified Data.List                as L
import           Data.Proxy               (Proxy (..))
import qualified Data.Text                as T
import           GHC.Conc                 (TVar, atomically, newTVarIO,
                                           readTVar, writeTVar)

import           Data.Aeson               (encode)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Server

import qualified Database
import qualified Environment
import qualified Logger


type Port = Int

type BazaarAPI
  = "health" :> Get '[JSON] T.Text

bazaarAPI :: Proxy BazaarAPI
bazaarAPI = Proxy

-- Handlers
healthHandler :: Handler T.Text
healthHandler = pure "OK"

server :: ServerT BazaarAPI Handler
server = healthHandler

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
    { hDB           :: Database.Handle -- ^ Database Handle
    , hLogger       :: Logger.Handle   -- ^ Logger Handle
    , hServerThread :: ThreadId        -- ^ Server threadId
    , hConfig       :: Config          -- ^ Server Config
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
  threadId <- forkIO $ run cPort (serve bazaarAPI server)

  -- Hack :( wait till the server is properly started
  threadDelay 10000

  return $ Handle databaseHandle loggerHandle threadId config

killServer :: Handle -> IO ()
killServer = killThread . hServerThread
