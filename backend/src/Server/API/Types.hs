{-# LANGUAGE OverloadedStrings #-}

module Server.API.Types
  ( PublicItem (..)
  , PublicKeyword (..)
  , PublicFeedback (..)
  , EmailAddress(..)
  )
  where

import           Data.Aeson
import qualified Data.Text      as T

import           Model

newtype PublicFeedback = PublicFeedback
  { fdbMessage :: T.Text }
  deriving (Show)

newtype EmailAddress
  = EmailAddress
    { emailAddress :: T.Text }
  deriving (Show)

instance FromJSON EmailAddress where
  parseJSON = withObject "EmailAddress" $ \o ->
    EmailAddress <$> o .: "email_address"

instance ToJSON EmailAddress where
  toJSON (EmailAddress email) = object [ "email_address"     .= email ]

instance FromJSON PublicFeedback where
  parseJSON = withObject "Feedback" $ \o ->
    PublicFeedback <$> o .: "message"

instance ToJSON PublicFeedback where
  toJSON (PublicFeedback msg) = object [ "message"     .= msg ]

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
