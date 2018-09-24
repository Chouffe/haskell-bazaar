{-# LANGUAGE ScopedTypeVariables #-}

module Server
  ( run
  , withServer
  )
  where

import           Control.Concurrent       (ThreadId, forkIO, killThread,
                                           threadDelay)
import           Control.Exception        (bracket)

import qualified Network.Wai.Handler.Warp as Warp

import qualified Database
import qualified Logger
import           Server.API               (app)
import qualified Server.Config

run
  :: Server.Config.Config
  -> Logger.Handle
  -> Database.Handle
  -> IO ()
run cfg loggerHandle databaseHandle =
  Warp.run (Server.Config.cPort cfg) (app handle)
  where
    handle :: Server.Config.Handle
    handle = Server.Config.new cfg loggerHandle databaseHandle

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
