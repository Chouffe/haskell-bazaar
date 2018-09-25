module Server.Client
  ( health
  , search
  , keywords
  )
  where

import qualified Data.Text        as T

import           Servant.API
import           Servant.Client   (ClientM, client)

import           Server.API       (bazaarAPI)
import           Server.API.Types


health :: ClientM T.Text
search :: Maybe T.Text -> ClientM [PublicItem]
keywords :: ClientM [PublicKeyword]
( health :<|>
  search :<|>
  keywords ) = client bazaarAPI
