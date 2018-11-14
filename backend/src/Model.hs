{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import qualified Data.Text              as T

import           Data.Aeson
import           Data.ByteString.Char8  as B8
import           Data.Time.Calendar     (Day)
import           Data.Time.Clock        (UTCTime)
import           Data.UUID              (UUID)
import qualified Data.UUID              as UUID
import           Database.Persist.Class (PersistField (..))
import           Database.Persist.Sql   (PersistFieldSql (..))
import           Database.Persist.TH
import           Database.Persist.Types (PersistValue (..), SqlType (..))

import qualified Model.Item             as ModelItem


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- TODO: add unique constraint on item
Item
    title       T.Text
    description T.Text Maybe
    url         T.Text
    itemType    ModelItem.Type
    createdAt   Day Maybe                      default=NULL
    uuid        UUID            sqltype=uuid   default=UUID_GENERATE_V4()
    deriving Show Eq

Author
    firstName   T.Text
    lastName    T.Text
    uuid        UUID            sqltype=uuid   default=UUID_GENERATE_V4()
    UniqueAuthor firstName lastName
    deriving Show

ItemAuthor
    itemId      ItemId
    authorId    AuthorId
    UniqueItemAuthor itemId authorId

Tag
    name        T.Text
    UniqueTagName name
    deriving Show

ItemTag
    itemId      ItemId
    tagId       TagId
    UniqueItemTag itemId tagId

Feedback
    message     T.Text
    createdAt   UTCTime
    deriving Show
|]

instance Ord Item where
  compare (Item t1 _ _ _ _ _) (Item t2 _ _ _ _ _) = compare t1 t2

instance ToJSON Author where
  toJSON (Author firstName lastName uuid) = object
    [ "firstName" .= firstName
    , "lastName"  .= lastName
    , "uuid"      .= uuid
    ]

instance ToJSON Tag where
  toJSON (Tag name) = object [ "name" .= name ]

-- Resource: http://bitemyapp.com/posts/2016-06-15-uuids-with-persistent-yesod.html
-- Note we're taking advantage of
-- PostgreSQL understanding UUID values,
-- thus "PersistDbSpecific"
instance PersistField UUID where
  toPersistValue u = PersistDbSpecific . B8.pack . UUID.toString $ u
  fromPersistValue (PersistDbSpecific t) =
    case UUID.fromString $ B8.unpack t of
      Just x  -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"
