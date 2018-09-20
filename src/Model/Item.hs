{-# LANGUAGE TemplateHaskell #-}

module Model.Item (Type(..)) where

import Database.Persist.TH

data Type
  = Article
  | Video
  | Tutorial
  | Book
  deriving (Show, Read, Eq)

derivePersistField "Type"
