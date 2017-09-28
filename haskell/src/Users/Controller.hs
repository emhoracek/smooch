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
  putStrLn (T.unpack $ T.intercalate "\n" (map (T.pack . show) users))
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
  let passwordsDontMatch =
        if password == passwordConfirmation
        then Nothing
        else Just ("password", "Your passwords don't match.")
  usernameTaken <-
     (fmap. fmap)
       (const ("username", "That username is already in use."))
       (getUserByUsername ctxt username)
  emailTaken <-
       (fmap . fmap)
       (const ("email", "That email is already in use."))
       (getUserByEmail ctxt email)
  let emailInvalid =
        if "@" `T.isInfixOf` email
        then Nothing
        else Just ("email", "Please enter a valid email.")
  let errors = catMaybes [passwordsDontMatch, usernameTaken, emailTaken, emailInvalid]
  if null errors
    then return $ Right $ NewUser username email password
    else return $ Left errors

createUserErrorSplices :: Substitutions Ctxt
createUserErrorSplices =
          subs [ ("usernameErrors", textFill "")
               , ("emailErrors", textFill "")
               , ("passwordErrors", textFill "")]
