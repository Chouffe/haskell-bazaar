{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server
  ( run
  , withServer
  )
  where

import           Control.Concurrent                   (ThreadId, forkIO,
                                                       killThread, threadDelay)
import           Control.Exception                    (bracket)
import           Data.Function                        ((&))
import           Data.Semigroup                       ((<>))
import qualified Data.Text                            as T

import           Network.Wai                          (Application)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.Cors          (simpleCors)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import qualified Database
import qualified Environment
import qualified Logger
import           Server.API                           (app)
import qualified Server.Config

middleware
  :: Environment.Environment
  -> Application
  -> Application
middleware env =
  case env of
    Environment.Test -> simpleCors
    Environment.Dev  -> simpleCors . logStdoutDev
    Environment.Prod -> logStdout

run
  :: Server.Config.Config
  -> Logger.Handle
  -> Database.Handle
  -> IO ()
run cfg loggerHandle databaseHandle =
  app handle &
  middleware (Server.Config.cEnvironment cfg) &
  Warp.runSettings appSettings

  where
    appSettings :: Warp.Settings
    appSettings =
      Warp.defaultSettings &
      Warp.setPort (Server.Config.cPort cfg) &
      Warp.setLogger (\r st mfs -> Logger.info loggerHandle $ display r st mfs)

    handle :: Server.Config.Handle
    handle = Server.Config.new cfg loggerHandle databaseHandle

    display r st mfs =
      "[Status: " <> T.pack (show st) <> "] " <>
      "[FileSize: " <> T.pack (show mfs) <> "] " <>
      T.pack (show r)

withServer
  :: Server.Config.Config
  -> Logger.Handle
  -> Database.Handle
  -> IO a
  -> IO a
withServer cfg loggerHandle databaseHandle io =
  bracket startBackgroundServer shutdownBackgroundServer (const io)

  where
    startBackgroundServer :: IO ThreadId
    startBackgroundServer = do
      tid <- forkIO $ run cfg loggerHandle databaseHandle
      -- Hack :( wait till the server is properly started
      threadDelay 100000
      return tid

    shutdownBackgroundServer :: ThreadId -> IO ()
    shutdownBackgroundServer = killThread
