module Server.Client
  ( health
  , search
  )
  where

import qualified Data.Text      as T

import           Servant.API
import           Servant.Client (ClientM, client)

import           Server.API     (bazaarAPI)


health :: ClientM T.Text
search :: Maybe T.Text -> ClientM T.Text
health :<|> search = client bazaarAPI
