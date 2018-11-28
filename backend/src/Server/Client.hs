module Server.Client
  ( health
  , itemUrl
  , allItems
  , feedback
  , search
  , searchTracking
  , keywords
  , subscribe
  )
  where

import qualified Data.Text        as T
import           Data.UUID        (UUID)

import           Servant.API
import           Servant.Client   (ClientM, client)

import           Server.API       (bazaarAPI)
import           Server.API.Types


health            :: ClientM T.Text
itemUrl           :: UUID -> Maybe T.Text -> ClientM T.Text
allItems          :: ClientM [PublicItem]
search            :: Maybe T.Text -> ClientM [PublicItem]
searchTracking    :: SearchTracking -> ClientM ()
keywords          :: ClientM [PublicKeyword]
feedback          :: PublicFeedback -> ClientM T.Text
subscribe         :: EmailAddress -> ClientM ()
( health           :<|>
  search           :<|>
  searchTracking   :<|>
  keywords         :<|>
  itemUrl          :<|>
  allItems         :<|>
  feedback         :<|>
  subscribe        ) = client bazaarAPI
