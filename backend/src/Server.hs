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

import           Network.Wai                          (Application, Middleware)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.Cors          (CorsResourcePolicy(..), simpleCors, cors, simpleMethods)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import qualified Database
import qualified Environment
import qualified Mailchimp
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
    Environment.Dev  -> apiDevCors . logStdoutDev
    Environment.Prod -> apiCors . logStdout

apiDevResourcePolicy :: CorsResourcePolicy
apiDevResourcePolicy = CorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = simpleMethods ++ ["OPTIONS"]
  , corsRequestHeaders = ["Authorization", "Content-Type"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }

apiResourcePolicy :: CorsResourcePolicy
apiResourcePolicy = CorsResourcePolicy
  { corsOrigins        = Just (["https://s3.amazonaws.com/haskell-bazaar/", "http://s3.amazonaws.com/haskell-bazaar/"], True)
  , corsMethods        = simpleMethods
  , corsRequestHeaders = []
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }

apiCors :: Middleware
apiCors  = cors $ const (Just apiResourcePolicy)

apiDevCors :: Middleware
apiDevCors = cors $ const (Just apiDevResourcePolicy)

run
  :: Server.Config.Config
  -> Logger.Handle
  -> Database.Handle
  -> Mailchimp.Handle
  -> IO ()
run cfg loggerHandle databaseHandle mailchimpHandle =
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
    handle = Server.Config.new cfg loggerHandle databaseHandle mailchimpHandle

    display r st mfs =
      "[Status: " <> T.pack (show st) <> "] " <>
      "[FileSize: " <> T.pack (show mfs) <> "] " <>
      T.pack (show r)

withServer
  :: Server.Config.Config
  -> Logger.Handle
  -> Database.Handle
  -> Mailchimp.Handle
  -> IO a
  -> IO a
withServer cfg loggerHandle databaseHandle mailchimpHandle io =
  bracket startBackgroundServer shutdownBackgroundServer (const io)

  where
    startBackgroundServer :: IO ThreadId
    startBackgroundServer = do
      tid <- forkIO $ run cfg loggerHandle databaseHandle mailchimpHandle
      -- Hack :( wait till the server is properly started
      threadDelay 100000
      return tid

    shutdownBackgroundServer :: ThreadId -> IO ()
    shutdownBackgroundServer = killThread
