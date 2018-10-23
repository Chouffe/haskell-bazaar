module Server.Client
  ( health
  , itemUrl
  , allItems
  , search
  , keywords
  )
  where

import qualified Data.Text        as T
import           Data.UUID        (UUID)

import           Servant.API
import           Servant.Client   (ClientM, client)

import           Server.API       (bazaarAPI)
import           Server.API.Types


health :: ClientM T.Text
itemUrl :: UUID -> ClientM T.Text
allItems :: ClientM [PublicItem]
search :: Maybe T.Text -> ClientM [PublicItem]
keywords :: ClientM [PublicKeyword]
( health   :<|>
  search   :<|>
  keywords :<|>
  itemUrl  :<|>
  allItems  ) = client bazaarAPI
