{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Logger
  ( Config (..)
  , config

  , Handle (..)
  , withHandle

  , Verbosity (..)
  , debug
  , info
  , warning
  , error
  )
  where

import           Control.Exception      (bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Time.Clock        (getCurrentTime)
import           Prelude                hiding (error)
import qualified System.Log.FastLogger  as FL

import           Environment            (Environment (..))

data Verbosity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)

data Config
  = Config
    { cPath      :: Maybe FilePath
    , cVerbosity :: Maybe Verbosity
    }
    deriving (Show, Eq)

config :: Environment -> Config
config Prod = Config { cVerbosity = Just Info, cPath = Nothing }
config Dev = Config { cVerbosity = Just Debug, cPath = Nothing }
config Test = Config { cVerbosity = Just Error, cPath = Nothing }

data Handle = Handle
  { hConfig    :: Config
  , hLoggerSet :: FL.LoggerSet
  }

withHandle :: MonadIO m => Config -> (Handle -> IO a) -> m a
withHandle cfg@Config{..} f = liftIO $
  bracket
  (case cPath of
     Nothing   -> FL.newStderrLoggerSet FL.defaultBufSize
     Just "-"  -> FL.newStderrLoggerSet FL.defaultBufSize
     Just path -> FL.newFileLoggerSet FL.defaultBufSize path)
  FL.rmLoggerSet
  (\l -> liftIO $ f Handle { hConfig = cfg, hLoggerSet = l })

log' :: FL.ToLogStr s => Handle -> Verbosity -> s -> IO ()
log' Handle {..} v x
  | v >= verbosity = do
    t <- getCurrentTime
    FL.pushLogStrLn hLoggerSet $ FL.toLogStr ("[" <> show v <> "][" <> show t <> "] ") <> FL.toLogStr x
  | otherwise = return ()

  where
    verbosity :: Verbosity
    verbosity = fromMaybe Debug (cVerbosity hConfig)

debug, info, warning, error :: FL.ToLogStr s => Handle -> s -> IO ()
debug h   = log' h Debug
info h    = log' h Info
warning h = log' h Warning
error h   = log' h Error
