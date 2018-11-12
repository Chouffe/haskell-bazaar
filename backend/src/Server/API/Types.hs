{-# LANGUAGE OverloadedStrings #-}

module Server.API.Types
  ( PublicItem (..)
  , PublicKeyword (..)
  , Feedback (..)
  )
  where

import           Data.Aeson
import qualified Data.Text      as T

import           Model

newtype Feedback = Feedback
  { fdbMessage :: T.Text }
  deriving (Show)

instance FromJSON Feedback where
  parseJSON = withObject "Feedback" $ \o ->
    Feedback <$> o .: "message"

instance ToJSON Feedback where
  toJSON (Feedback msg) = object [ "message"     .= msg ]


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
    , "uuid"        .= itemUuid item
    , "type"        .= T.pack (show (itemItemType item))
    , "authors"     .= toJSON authors
    , "tags"        .= toJSON tags
    , "created_at"  .= itemCreatedAt item
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
    , "uuid"      .= authorUuid author
    ]

-- TODO
instance FromJSON PublicKeyword where
  parseJSON = undefined
