{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import qualified Data.Text           as T

import           Data.Time           (UTCTime)
import           Database.Persist.TH

import qualified Model.Item          as ModelItem

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Item
    title       T.Text
    description T.Text Maybe
    url         T.Text
    itemType    ModelItem.Type
    createdAt   UTCTime default=CURRENT_TIME
    deriving Show Eq

Author
    firstName T.Text
    lastName  T.Text
    deriving Show

ItemAuthor
    itemId   ItemId
    authorId AuthorId
    UniqueItemAuthor itemId authorId

Tag
    name T.Text
    deriving Show

ItemTag
    itemId ItemId
    tagId  TagId
    UniqueItemTag itemId tagId
|]

instance Ord Item where
  compare (Item t1 _ _ _ _) (Item t2 _ _ _ _) = compare t1 t2
