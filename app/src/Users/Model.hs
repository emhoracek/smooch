{-# LANGUAGE OverloadedStrings #-}

module Users.Model where

import           Control.Lens                       ((^.))
import           Data.Maybe                         (listToMaybe)
import           Data.Pool                          (withResource)
import           Data.Text                          (Text)
import           Data.Time.Clock                    (UTCTime)
import qualified Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import           Ctxt
import           Session

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

getUsers :: Ctxt -> IO [User]
getUsers ctxt =
  withResource (ctxt ^. pool) (\conn ->
    PG.query_
     conn
     "SELECT id, username, email, created_at, updated_at FROM users"
       :: IO [ User ])

createUser :: Ctxt -> NewUser -> IO Bool
createUser ctxt newUser = (==) 1 <$>
  withResource (ctxt ^. pool) (\conn ->
    PG.execute
     conn
     "INSERT INTO users (username, email, password) VALUES (?, ?, ?)"
     newUser)

getUserByUsername :: Ctxt -> Text -> IO (Maybe User)
getUserByUsername ctxt username = listToMaybe <$>
  withResource (ctxt ^. pool) (\conn ->
    PG.query
     conn
     "SELECT id, username, email, created_at, updated_at FROM users\
     \ WHERE username = ?"
     (PG.Only username)
       :: IO [ User ])

authenticateUser :: Ctxt -> Text -> Text -> IO (Maybe User)
authenticateUser ctxt username password = listToMaybe <$>
    withResource (ctxt ^. pool) (\conn ->
    PG.query
     conn
     "SELECT id, username, email, created_at, updated_at FROM users\
     \ WHERE username = ? AND crypt(?, password) = password"
     (username, password)
       :: IO [ User ])

getUserByEmail :: Ctxt -> Text -> IO (Maybe User)
getUserByEmail ctxt email = listToMaybe <$>
  withResource (ctxt ^. pool) (\conn ->
    PG.query
     conn
     "SELECT id, username, email, created_at, updated_at FROM users\
     \ WHERE email = ?"
     (PG.Only email)
       :: IO [ User ])

getLoggedInUser :: Ctxt -> IO (Maybe User)
getLoggedInUser ctxt = do
  case ctxt ^. skipLogin of
    Just username -> getUserByUsername ctxt username
    Nothing -> do
      mUsername <- getFromSession ctxt "user"
      case mUsername of
        Just username -> getUserByUsername ctxt username
        Nothing -> return Nothing

setLoggedInUser :: Ctxt -> User -> IO ()
setLoggedInUser ctxt =
  setInSession ctxt "user" . Just . userUsername

setLoggedOut :: Ctxt -> IO ()
setLoggedOut ctxt =
  setInSession ctxt "user" Nothing
