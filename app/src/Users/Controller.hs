{-# LANGUAGE OverloadedStrings #-}

module Users.Controller where

import           Data.Maybe         (catMaybes)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock    (UTCTime (..), secondsToDiffTime)
import           Network.HTTP.Types (StdMethod (..))
import           Network.Wai        (Response)
import           Web.Fn
import           Web.Larceny        hiding (renderWith)

import           Ctxt
import           Dolls.Controller
import           Users.Model
import           Users.View
import           Dolls.View

usersRoutes :: Ctxt -> IO (Maybe Response)
usersRoutes ctxt =
  route ctxt [ end ==> usersHandler
             , method POST // path "create"
                           // param "username"
                           // param "email"
                           // param "password"
                           // param "password-confirmation" !=> usersCreateHandler
             , segment ==> requireAuthentication loggedInUserRoutes ]

usersHandler :: Ctxt -> IO (Maybe Response)
usersHandler ctxt = do
  users <- getUsers ctxt
  renderWith ctxt ["users", "index"] (usersSplices users)

requireAuthentication :: (Ctxt -> User -> k -> IO (Maybe Response))
                      -> Ctxt -> k -> IO (Maybe Response)
-- This is a weird type signature! Here's what is going on.
-- `k` is the type of any params or segment arguments.
-- For example, `(segment ==> requireAuthentication userHandler)`
-- passes one `Text` argument, so `k` is `Text`. But if it was
-- `(segment // path "id" ==> requireAuthentication otherHandler)`
-- then `k` might be `Text -> Int`. Keeping `k` abstract lets us
-- handle all sorts of different types of arguments to our handlers.
requireAuthentication handler ctxt k = do
  mUser <- getLoggedInUser ctxt
  case mUser of
    Just user -> handler ctxt user k
    Nothing -> errText "you're not logged in"

loggedInUserRoutes :: Ctxt -> User -> Text -> IO (Maybe Response)
loggedInUserRoutes ctxt loggedInUser username = do
  if userUsername loggedInUser == username
    then route ctxt [ end ==> userHandler loggedInUser
                    , path "upload" // method POST
                                    // file "kissfile"
                                    !=> userUploadHandler loggedInUser
                    , path "sets" // segment
                                  // end
                                  ==> userDollHandler loggedInUser ]
    else return Nothing

userHandler ::  User -> Ctxt -> IO (Maybe Response)
userHandler loggedInUser ctxt = do
  renderWith ctxt ["users", "show"] (userSplices loggedInUser)

usersCreateHandler :: Ctxt -> Text -> Text -> Text -> Text -> IO (Maybe Response)
usersCreateHandler ctxt username email password passwordConfirmation = do
  eNewUser <-
    validateNewUser ctxt username email password passwordConfirmation
  case eNewUser of
    Left errors ->
      renderWith ctxt ["index"] (errorSplices errors)
    Right newUser -> do
      success <- createUser ctxt newUser
      if success
         then do let fakeTime = UTCTime (fromGregorian 2018 8 13) (secondsToDiffTime 0)
                 -- 'setLoggedInUser' only requires the username, so we create a
                 -- temporary 'User' to avoid making a potentially expensive
                 -- call to the database.
                 setLoggedInUser ctxt $ User 0 username email fakeTime fakeTime
                 redirect ("/users/" <> username)
         else errHtml "couldn't create user"
 where errorSplices errors =
         newErrorSplices errors <> createUserErrorSplices <> linkUploadSplices
       newErrorSplices errors =
         subs $ map (\(k,v) -> (k <> "Errors", textFill v)) errors

type Errors = [(Text, Text)]

validateNewUser :: Ctxt -> Text -> Text -> Text -> Text -> IO (Either Errors NewUser)
validateNewUser ctxt username email password passwordConfirmation = do
  let pwMissing =
        errBool "password" "Please enter a password."
          (password == "")
  let emailMissing =
        errBool "email" "Please enter your email."
          (email == "")
  let usernameMissing =
        errBool "username" "Please enter a username."
          (username == "")
  let passwordsDontMatch =
        errBool "password" "Your passwords don't match."
                (password /= passwordConfirmation)
  usernameTaken <-
     errMaybe "username" "That username is already in use."
       (getUserByUsername ctxt username)
  emailTaken <-
     errMaybe "email" "That email is already in use."
       (getUserByEmail ctxt email)
  let emailInvalid =
        errBool "email" "Please enter a valid email."
          (not $ "@" `T.isInfixOf` email)
  let errors = catMaybes [passwordsDontMatch, usernameTaken,
                          emailTaken, emailInvalid, pwMissing,
                          emailMissing, usernameMissing]
  if null errors
    then return $ Right $ NewUser username email password
    else return $ Left errors
  where errBool field msg cond =
          if cond then Just (field, msg) else Nothing
        errMaybe field msg maybeAction =
          (fmap . fmap)
          (const (field, msg))
          maybeAction
