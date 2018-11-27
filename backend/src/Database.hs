{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database
  ( Config
  , config

  , Handle
  , withHandle

  , runDatabase

  -- DB queries
  , allItems
  , allItemsWithTags
  , itemUrlByUUID
  , itemByUUID
  , itemClick
  , keywords
  , searchByAuthor
  , searchByTagName
  , searchEvent
  , feedback
  )
  where

import           Control.Arrow               ((***))
import           Control.Exception           (bracket)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (LoggingT, runStdoutLoggingT)
import qualified Data.Map                    as M
import           Data.Maybe                  (maybe, listToMaybe)
import           Data.Pool                   (Pool, destroyAllResources,
                                              withResource)
import           Data.Semigroup              ((<>))
import qualified Data.Text                   as T
import           Data.UUID                   (UUID)
import           Data.Time.Clock             (getCurrentTime)
import           Database.Esqueleto          (InnerJoin (..), distinct, from,
                                              ilike, in_, limit, on, select,
                                              val, unValue, valList, where_, (%), (++.),
                                              (==.), (^.))

import           Database.Persist            (Entity (..), insert_)
import           Database.Persist.Postgresql (ConnectionString,
                                              createPostgresqlPool)
import           Database.Persist.Sql        (SqlBackend, SqlPersistT,
                                              runSqlConn)
import           Network.Socket              (SockAddr)

import           Environment                 (Environment (..))
import           Model
import           Server.API.Types

data Config
  = Config
    { cConnectionString :: ConnectionString
    , cPoolConnections  :: Int
    , cEnvironnment     :: Environment
    }
  deriving (Eq, Show)

data Handle
  = Handle
    { hPool   :: Pool SqlBackend
    , hConfig :: Config
    }

config :: Environment -> Config
config Test = Config "host=localhost port=5432 user=haskellbazaar dbname=haskellbazaar password=password" 1 Test
config Dev  = Config "host=localhost port=5432 user=haskellbazaar dbname=haskellbazaar password=password" 1 Dev
-- TODO: get from ENV variable
config Prod = Config "host=postgres port=5432 user=haskellbazaar dbname=haskellbazaar password=password" 10 Prod

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle cfg = bracket
  (mkHandle cfg)
  (\Handle {..} -> destroyAllResources hPool)

mkHandle :: Config -> IO Handle
mkHandle cfg@Config {..} = do
  pool <- runStdoutLoggingT $ createPostgresqlPool cConnectionString cPoolConnections
  return $ Handle pool cfg

runDatabase :: Handle -> SqlPersistT (LoggingT IO) a -> IO a
runDatabase Handle {..} query =
  runStdoutLoggingT $ withResource hPool (runSqlConn query)

allItems
  :: (MonadIO m)
  => (SqlPersistT m) [PublicItem]
allItems = do
    entities <- select . from $ \item -> do
      limit 1000
      return item
    hydrateItems entities

searchByTagName
  :: (MonadIO m)
  => T.Text
  -> (SqlPersistT m) [PublicItem]
searchByTagName query = do
  itemEntities <- itemsByTagName query
  if null itemEntities
  then pure []
  else hydrateItems itemEntities

searchByAuthor
  :: (MonadIO m)
  => T.Text
  -> (SqlPersistT m) [PublicItem]
searchByAuthor query = do
  itemEntities <- itemsByAuthorName query
  if null itemEntities
  then pure []
  else hydrateItems itemEntities

itemsByAuthorName
  :: (MonadIO m)
  => T.Text    -- ^ Tag Name
  -> (SqlPersistT m) [Entity Item]
itemsByAuthorName query =
  select $ distinct $
    from $ \(items' `InnerJoin` itemAuthors' `InnerJoin` authors') -> do
      on (itemAuthors' ^. ItemAuthorAuthorId ==. authors' ^. AuthorId)
      on (items' ^. ItemId ==. itemAuthors' ^. ItemAuthorItemId)
      where_
        ( authors' ^. AuthorFirstName ++. val " " ++. authors' ^. AuthorLastName
        `ilike` (%) ++. val query ++. (%)
        )
      limit 100
      return items'

itemsByTagName
  :: (MonadIO m)
  => T.Text    -- ^ Tag Name
  -> (SqlPersistT m) [Entity Item]
itemsByTagName query =
  select $ distinct $
    from $ \(items' `InnerJoin` itemTags' `InnerJoin` tags') -> do
      on (itemTags' ^. ItemTagTagId ==. tags' ^. TagId)
      on (items' ^. ItemId ==. itemTags' ^. ItemTagItemId)
      where_ (tags' ^. TagName ==. val query)
      limit 100
      return items'

hydrateItems
  :: (MonadIO m)
  => [Entity Item]
  -> (SqlPersistT m) [PublicItem]
hydrateItems itemEntities = do

  authorsAndTags :: [(Entity Item, Entity Author, Entity Tag)] <-
    select $ distinct $
      from $ \ ( items'
               `InnerJoin` itemAuthors'
               `InnerJoin` authors'
               `InnerJoin` itemTags'
               `InnerJoin` tags') -> do
        on (itemTags' ^. ItemTagTagId ==. tags' ^. TagId)
        on (items' ^. ItemId ==. itemTags' ^. ItemTagItemId)
        on (itemAuthors' ^. ItemAuthorAuthorId ==. authors' ^. AuthorId)
        on (items' ^. ItemId ==. itemAuthors' ^. ItemAuthorItemId)
        where_ $ items' ^. ItemId `in_` valList itemIds
        return (items', authors', tags')

  let itemTagsAuthors = toMap authorsAndTags

  return $ do
    itemEntity <- itemEntities
    let mAuthorAndTag = M.lookup itemEntity itemTagsAuthors
    return PublicItem
      { piItem    = entityVal itemEntity
      , piTags    = entityVal <$> maybe [] snd mAuthorAndTag
      , piAuthors = entityVal <$> maybe [] fst mAuthorAndTag
      }

    where
      itemIds :: [ItemId]
      itemIds = entityKey <$> itemEntities

      toMap :: [(Entity Item, a, b)] -> M.Map (Entity Item) ([a], [b])
      toMap xs =
        M.fromListWith
          (\(as, ts) (as', ts') -> (as <> as', ts <> ts'))
          ((\(i, a, t) -> (i, ([a], [t]))) <$> xs)

-- TODO: how to get items without tags? Using a LeftOuterJoin but I am getting type errors
allItemsWithTags
  :: (MonadIO m)
  => (SqlPersistT m) (M.Map Item [Tag])
allItemsWithTags = do

    tuples <- select $ distinct $
      from $ \(items `InnerJoin` itemTags `InnerJoin` tags) -> do
        on (itemTags ^. ItemTagTagId ==. tags ^. TagId)
        on (items ^. ItemId ==. itemTags ^. ItemTagItemId)
        limit 100
        return (items, tags)
    return $ toMap $ (entityVal *** entityVal) <$> tuples

    where
      toMap :: [(Item, Tag)] -> M.Map Item [Tag]
      toMap xs = M.fromListWith (++) (fmap (\(i, t) -> (i, [t])) xs)

allTags
  :: MonadIO m
  => (SqlPersistT m) [Tag]
allTags = do
  results <- select . from $ return
  return (entityVal <$> results)

allAuthors
  :: MonadIO m
  => (SqlPersistT m) [Author]
allAuthors = do
  results <- select . from $ return
  return (entityVal <$> results)

itemUrlByUUID
  :: MonadIO m
  => UUID
  -> (SqlPersistT m) (Maybe (ItemId, T.Text))
itemUrlByUUID uuid = do
  results <-
    select . from $ \item -> do
      where_ $ item ^. ItemUuid ==. val uuid
      return (item ^. ItemId, item ^. ItemUrl)
  return
    $ listToMaybe
    $ (unValue *** unValue) <$> results

itemByUUID
  :: MonadIO m
  => UUID
  -> (SqlPersistT m) (Maybe (Entity Item))
itemByUUID uuid = do
  results <-
    select . from $ \item -> do
      where_ $ item ^. ItemUuid ==. val uuid
      return item
  return $ listToMaybe results

keywords
  :: MonadIO m
  => (SqlPersistT m) [PublicKeyword]
keywords = do
  tags    <- allTags
  authors <- allAuthors
  return $ (PublicTag <$> tags) <> (PublicAuthor <$> authors)

feedback
  :: MonadIO m
  => PublicFeedback
  -> (SqlPersistT m) ()
feedback (PublicFeedback msg) = do
  currentTime <- liftIO getCurrentTime
  insert_ (Feedback msg currentTime)

searchEvent
  :: MonadIO m
  => SockAddr
  -> T.Text
  -> (SqlPersistT m) ()
searchEvent sockAddr searchQuery = do
  currentTime <- liftIO getCurrentTime
  insert_ (SearchEvent searchQuery (show sockAddr) currentTime)

itemClick
  :: MonadIO m
  => SockAddr
  -> ItemId
  -> Maybe T.Text
  -> (SqlPersistT m) ()
itemClick sockAddr itemId mSearchQuery = do
  currentTime <- liftIO getCurrentTime
  insert_ (ItemClick itemId currentTime (show sockAddr) mSearchQuery)
