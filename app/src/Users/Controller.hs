{-# LANGUAGE OverloadedStrings #-}

module Users.Controller where

import           Data.Maybe         (catMaybes)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Network.HTTP.Types (StdMethod (..))
import           Network.Wai        (Response)
import           Web.Fn
import           Web.Larceny        hiding (renderWith)

import           Ctxt
import           Users.Model
import           Users.View

userRoutes :: Ctxt -> IO (Maybe Response)
userRoutes ctxt =
  route ctxt [ (end ==> usersHandler)
             , (method POST // path "create"
                            // param "username"
                            // param "email"
                            // param "password"
                            // param "password-confirmation" !=> usersCreateHandler)]

usersHandler :: Ctxt -> IO (Maybe Response)
usersHandler ctxt = do
  users <- getUsers ctxt
  renderWith ctxt ["users", "index"] (usersSplices users)

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
         then okHtml "created!"
         else errHtml "couldn't create user"
 where errorSplices errors =
         newErrorSplices errors <> createUserErrorSplices
       newErrorSplices errors =
         (subs $ map (\(k,v) -> (k <> "Errors", textFill v)) errors)

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

createUserErrorSplices :: Substitutions Ctxt
createUserErrorSplices =
          subs [ ("usernameErrors", textFill "")
               , ("emailErrors", textFill "")
               , ("passwordErrors", textFill "")]
