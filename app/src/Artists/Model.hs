{-# LANGUAGE OverloadedStrings #-}

module Artists.Model where

import           Control.Lens                       ((^.))
import           Data.Maybe                         (listToMaybe)
import           Data.Pool                          (withResource)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import qualified Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import           Ctxt
import           Session
import Users.Model (User(userId))


data Artist = Artist {
    artistId :: Int
  , artistName :: Text
  , artistEmail :: Maybe Text
  , artistWebsite :: Maybe Text
  , artistUserId :: Maybe Int
  , dollCreatedAt :: UTCTime
  , dollUpdatedAt :: UTCTime
}

instance FromRow Artist where
  fromRow = Artist <$> field <*> field <*> field <*> field <*> field
                   <*> field <*> field

data NewArtist = NewArtist {
    newArtistName :: Text
  , newArtistEmail :: Maybe Text
  , newArtistWebsite :: Maybe Text
  , newArtistUserId :: Maybe Text
} deriving (Eq, Show)

instance ToRow NewArtist where
  toRow (NewArtist name email website uid) =
    [ toField name, toField email, toField website, toField uid ]

artistQuery :: PG.Query
artistQuery = "SELECT artists.id, artists.name, artists.email, \
  \ artists.website, artists.user_id, artists.created_at, \
  \ artists.updated_at FROM arists "

getArtists :: Ctxt -> IO [Artist]
getArtists ctxt =
  withResource (ctxt ^. pool) (\conn ->
    PG.query_ conn artistQuery :: IO [ Artist ])

createArtist:: Ctxt -> NewArtist -> IO Bool
createArtist ctxt newArtist = (==) 1 <$>
  withResource (ctxt ^. pool) (\conn ->
    PG.execute
     conn
     "INSERT INTO dolls (name, email, website, user_id) VALUES (?, ?, ?, ?)"
     newArtist)

getArtistById :: Ctxt -> Text -> IO (Maybe Artist)
getArtistById ctxt aId = listToMaybe <$>
  withResource (ctxt ^. pool) (\conn ->
    PG.query
     conn
     (artistQuery <> " WHERE id = ?")
     (PG.Only aId)
       :: IO [ Artist ])

addArtistToDoll:: Ctxt -> Int -> Int -> IO Bool
addArtistToDoll ctxt aId dId = (==) 1 <$>
  withResource (ctxt ^. pool) (\conn ->
    PG.execute
     conn
     "INSERT INTO doll_artists (artist_id, doll_id) VALUES (?, ?)"
     (aId, dId))

getArtistsForDoll :: Ctxt -> Int -> IO [Artist]
getArtistsForDoll ctxt dId =
  withResource (ctxt ^. pool) (\conn ->
    PG.query
      conn
      (artistQuery <> " JOIN doll_artists AS da ON da.artist_id = artists.id WHERE da.doll_id = ?")
      (PG.Only dId)
      :: IO [ Artist ])