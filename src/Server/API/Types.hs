{-# LANGUAGE OverloadedStrings #-}

module Server.API.Types
  ( PublicItem (..)
  , PublicKeyword (..)
  )
  where

import           Data.Aeson
import qualified Data.Text      as T

import           Model

data PublicItem =
  PublicItem
  { piItem    :: Item
  , piAuthors :: [Author]
  , piTags    :: [Tag]
  }
  deriving (Show)

instance ToJSON PublicItem where
  toJSON (PublicItem item authors tags) = object
    [ "title"       .= itemTitle item
    , "description" .= itemDescription item
    , "url"         .= itemUrl item
    , "type"        .= T.pack (show (itemItemType item))
    , "authors"     .= toJSON authors
    , "tags"        .= toJSON tags
    ]

-- TODO
instance FromJSON PublicItem where
  parseJSON = undefined

data PublicKeyword
  = PublicTag Tag
  | PublicAuthor Author
  deriving (Show)

instance ToJSON PublicKeyword where
  toJSON (PublicTag tag)       = object
    [ "kind" .= ("tag" :: T.Text)
    , "name" .= tagName tag
    ]
  toJSON (PublicAuthor author) = object
    [ "kind"      .= ("author" :: T.Text)
    , "firstName" .= authorFirstName author
    , "lastName"  .= authorLastName author
    ]

-- TODO
instance FromJSON PublicKeyword where
  parseJSON = undefined
