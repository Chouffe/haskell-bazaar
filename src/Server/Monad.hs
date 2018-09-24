{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Server.Monad
  ( App
  , runAppT
  , transformAppToHandler
  )
  where

import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)

import           Servant.Server             ((:~>) (..), Handler (..),
                                             ServantErr (..))

import qualified Server.Config


newtype AppT m a
  = AppT { runAppT :: ReaderT Server.Config.Handle (ExceptT ServantErr m) a }
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Server.Config.Handle
    , MonadError ServantErr
    , MonadIO
    )

type App = AppT IO

transformAppToHandler
  :: Server.Config.Handle
  -> App :~> Handler
transformAppToHandler handle =
  NT $ \(AppT action) -> Handler $ runReaderT action handle
