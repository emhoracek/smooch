{-# LANGUAGE OverloadedStrings #-}

module Users.Model where

import           Data.Int                           (Int64)
import           Data.Maybe                         (listToMaybe)
import           Data.Pool                          (Pool, withResource)
import           Data.Text                          (Text)
import           Data.Time.Clock                    (UTCTime)
import qualified Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

data User = User { userId        :: Int
                 , userUsername  :: Text
                 , userEmail     :: Text
                 , userCreatedAt :: UTCTime
                 , userUpdatedAt :: UTCTime } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field

data NewUser =
  NewUser { newUserUsername :: Text
          , newUserEmail    :: Text
          , newUserPassword :: Text } deriving (Eq, Show)

instance FromRow NewUser where
  fromRow = NewUser <$> field <*> field <*> field

instance ToRow NewUser where
  toRow (NewUser username email password) =
    [toField username, toField email, toField password ]

getUsers :: Pool PG.Connection -> IO [User]
getUsers pgpool =
  withResource pgpool (\conn ->
    PG.query_
     conn
     "SELECT id, username, email, created_at, updated_at FROM users"
       :: IO [ User ])

createUser :: Pool PG.Connection -> NewUser -> IO Int64
createUser pgpool newUser =
  withResource pgpool (\conn ->
    PG.execute
     conn
     "INSERT INTO users (username, email, password) VALUES (?, ?, ?)"
     newUser)

getUserByUsername :: Pool PG.Connection -> Text -> IO (Maybe User)
getUserByUsername pgpool username = listToMaybe <$>
  withResource pgpool (\conn ->
    PG.query
     conn
     "SELECT id, username, email, created_at, updated_at FROM users\
     \ WHERE username = ?"
     (PG.Only username)
       :: IO [ User ])

authenticateUser :: Pool PG.Connection -> Text -> Text -> IO (Maybe User)
authenticateUser pgpool username password = listToMaybe <$>
    withResource pgpool (\conn ->
    PG.query
     conn
     "SELECT id, username, email, created_at, updated_at FROM users\
     \ WHERE username = ? AND crypt(?, password) = password"
     (username, password)
       :: IO [ User ])
