{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Servant.API.ContentTypesExtended
  ( module Servant.API.ContentTypes
  , HTML
  , RawHtml(..)
  )
  where

import qualified Data.ByteString.Lazy     as BS

import Servant.API.ContentTypes
import           Network.HTTP.Media       ((//), (/:))

data HTML
newtype RawHtml = RawHtml { unRaw :: BS.ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ =  unRaw
