{-# LANGUAGE OverloadedStrings #-}

module Server.API.Types
  ( PublicItem(..)
  , PublicKeyword(..)
  )
  where

import           Data.Aeson
import qualified Data.Text      as T
-- import qualified Data.Char  as C

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
    [ "kind" .= ("author" :: T.Text)
    , "firstName" .= authorFirstName author
    , "lastName" .= authorLastName author
    ]

instance FromJSON PublicKeyword where
  parseJSON = undefined
