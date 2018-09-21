{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server.Monad
  ( AppConfig
  , AppM
  , runAppM
  , transformAppMToHandler
  , transformAppMToHandlerNT
  )
  where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)

import           Servant.Server (Handler, (:~>) (..))

import qualified Database
import qualified Environment
import qualified Logger

type AppConfig = (Logger.Handle, Database.Handle)

newtype AppM a =
  AppM (ReaderT AppConfig IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppConfig
    )

runAppM :: AppConfig -> AppM a -> IO a
runAppM config (AppM action) = runReaderT action config

-- TODO: Improve Error Handling with Handler
transformAppMToHandler
  :: AppConfig
  -> AppM a
  -> Handler a
transformAppMToHandler config action =
  liftIO (runAppM config action)

transformAppMToHandlerNT
  :: AppConfig
  -> AppM :~> Handler
transformAppMToHandlerNT config =
  NT $ \action -> liftIO (runAppM config action)

-- transformAppToHandler sqliteInfo redisInfo = NT $ \action -> do
--   result <- liftIO $ handler `handle` (runAppAction sqliteInfo redisInfo action)
--   Handler $ either throwError return result
