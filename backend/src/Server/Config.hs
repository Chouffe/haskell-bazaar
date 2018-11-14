module Server.Config
  ( Port
  , port

  , Config
  , config
  , cPort
  , cEnvironment

  , Handle
  , new
  , hDB
  , hLogger
  , hConfig
  , hMailchimp
  )
  where

import qualified Database
import qualified Environment
import qualified Logger
import qualified Mailchimp


type Port = Int

port :: Environment.Environment -> Port
port Environment.Test = 8003
port Environment.Dev  = 8002
port Environment.Prod = 8001

data Config
  = Config
    { cEnvironment  :: Environment.Environment -- ^ Environment
    , cPort         :: Port                    -- ^ Port to run the server on
    }
  deriving (Eq, Show)

config :: Environment.Environment -> Config
config env = Config env (port env)

data Handle
  = Handle
    { hDB           :: Database.Handle   -- ^ Database Handle
    , hLogger       :: Logger.Handle     -- ^ Logger Handle
    , hMailchimp    :: Mailchimp.Handle  -- ^ Mailchimp Handle
    , hConfig       :: Config            -- ^ Server Config
    }

new
  :: Config
  -> Logger.Handle
  -> Database.Handle
  -> Mailchimp.Handle
  -> Handle
new cfg loggerHandle databaseHandle mailchimpHandle =
  Handle databaseHandle loggerHandle mailchimpHandle cfg
