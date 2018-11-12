module Server.Config
  ( Port
  , port

  , Config
  , config
  , cPort
  , cEnvironment
  , cPathFeedback

  , Handle
  , new
  , hDB
  , hLogger
  , hConfig
  )
  where

import qualified Database
import qualified Environment
import qualified Logger


type Port = Int

port :: Environment.Environment -> Port
port Environment.Test = 8003
port Environment.Dev  = 8002
port Environment.Prod = 8001

filepathFeedback :: Environment.Environment -> FilePath
filepathFeedback Environment.Prod = "/home/ubuntu/haskellbazaar-feedback-data.txt"
filepathFeedback _ = "/home/chouffe/haskellbazaar-feedback-data.txt"

data Config
  = Config
    { cEnvironment  :: Environment.Environment -- ^ Environment
    , cPort         :: Port                    -- ^ Port to run the server on
    , cPathFeedback :: FilePath                -- ^ FilePath to save the feedback
    }
  deriving (Eq, Show)

config :: Environment.Environment -> Config
config env = Config env (port env) (filepathFeedback env)

data Handle
  = Handle
    { hDB           :: Database.Handle  -- ^ Database Handle
    , hLogger       :: Logger.Handle    -- ^ Logger Handle
    , hConfig       :: Config           -- ^ Server Config
    }

new
  :: Config
  -> Logger.Handle
  -> Database.Handle
  -> Handle
new cfg loggerHandle databaseHandle =
  Handle databaseHandle loggerHandle cfg
