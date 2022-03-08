{-# LANGUAGE OverloadedStrings #-}

module Dolls.Model where

import           Control.Lens                       ((^.))
import qualified Data.ByteString                    as BS
import           Data.Maybe                         (listToMaybe)
import           Data.Pool                          (withResource)
import qualified Data.Text                          as T
import           Data.Text (Text)
import           Data.Time (UTCTime)
import qualified Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import           Ctxt

data Doll = Doll {
    dollId :: Int
  , dollName :: Text
  , dollOtakuWorldUrl :: Maybe Text
  , dollHash :: BS.ByteString
  , dollLocation :: Maybe Text
  , dollError :: Maybe Text
  , dollCreatedAt :: UTCTime
  , dollUpdatedAt :: UTCTime
} deriving (Eq, Show)

instance FromRow Doll where
  fromRow = Doll <$> field <*> field <*> field <*> field <*> field
                 <*> field <*> field <*> field

data NewDoll = NewDoll {
    newDollName :: Text
  , newDollOtakuWorldUrl :: Maybe Text
  , newDollHash :: BS.ByteString
  , newDollLocation :: Maybe Text
  , newDollError :: Maybe Text
} deriving (Eq, Show)

instance ToRow NewDoll where
  toRow (NewDoll name owurl hash location err) =
    [ toField name, toField owurl, toField hash, toField location
    , toField err ]

dollLocationOrErr :: Doll -> Either Text FilePath
dollLocationOrErr doll =
  case (dollLocation doll, dollError doll) of
    (Just loc, _)       -> Right (T.unpack loc)
    (Nothing, Just err) -> Left err
    (Nothing, Nothing)  -> Left "Doll not found"

dollQuery :: PG.Query
dollQuery = "SELECT dolls.id, dolls.name, dolls.otakuworld_url, dolls.hash, \
  \ dolls.location, dolls.error, dolls.created_at, dolls.updated_at FROM dolls"

getDolls :: Ctxt -> IO [Doll]
getDolls ctxt =
  withResource (ctxt ^. pool) (\conn ->
    PG.query_ conn dollQuery :: IO [ Doll ])

createDoll:: Ctxt -> NewDoll -> IO Bool
createDoll ctxt newDoll = (==) 1 <$>
  withResource (ctxt ^. pool) (\conn ->
    PG.execute
     conn
     "INSERT INTO dolls (name, otakuworld_url, hash, location, error) VALUES (?, ?, ?, ?, ?)"
     newDoll)

getDollByOWUrl :: Ctxt -> Text -> IO (Maybe Doll)
getDollByOWUrl ctxt owUrl = listToMaybe <$>
  withResource (ctxt ^. pool) (\conn ->
    PG.query
     conn
     (dollQuery <> " WHERE otakuworld_url = ?")
     (PG.Only owUrl)
       :: IO [ Doll ])

getDollByHash :: Ctxt -> BS.ByteString  -> IO (Maybe Doll)
getDollByHash ctxt hash = listToMaybe <$>
  withResource (ctxt ^. pool) (\conn ->
    PG.query
     conn
     (dollQuery <> " WHERE hash = ?")
     (PG.Only hash)
       :: IO [ Doll ])

getDollsForArtist :: Ctxt -> Int -> IO [Doll]
getDollsForArtist ctxt aId =
  withResource (ctxt ^. pool) (\conn ->
    PG.query
      conn
      (dollQuery <> " JOIN doll_artists AS da ON da.doll_id = dolls.id WHERE da.artist_id = ?")
      (PG.Only aId)
      :: IO [ Doll ])

-- We're going to treat OtakuWorld filenames as canonical for now, so when we
-- update a file that has already been uploaded with a different name, we also
-- make sure that the name is the same as the one in the OW URL.
updateDollWithUrl:: Ctxt -> Doll -> String -> Text -> IO (Maybe Doll)
updateDollWithUrl ctxt doll name owUrl =
  let updatedDoll = doll { dollName = T.pack name
                         , dollOtakuWorldUrl = Just owUrl } in
  if updatedDoll == doll
    then return (Just updatedDoll)
    else
      listToMaybe <$> withResource (ctxt ^. pool) (\conn ->
        PG.query
        conn
        "UPDATE dolls SET name = ?, otakuworld_url = ? WHERE id = ? \
        \ RETURNING id, name, otakuworld_url, hash, location, error, created_at, updated_at"
        (dollName updatedDoll, dollOtakuWorldUrl updatedDoll, dollId doll))