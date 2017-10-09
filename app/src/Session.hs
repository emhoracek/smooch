{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Session where

import           Control.Lens            ((^.))
import           Control.Monad.State     (StateT, get, liftIO)
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Vault.Lazy         as V
import           Network.Wai             (Middleware, Response, vault)
import           Network.Wai.Session     (genSessionId, withSession)
import           Network.Wai.Session.Map (mapStore)
import           Web.Cookie              (def)
import           Web.Fn
import           Web.Larceny             hiding (renderWith)

import           Ctxt
import           Users.Model

sessionMiddleware :: V.Key SmoochSession -> IO Middleware
sessionMiddleware vaultKey = do
  let setCookie = def
  sessionStore <- mapStore genSessionId
  return $ withSession sessionStore  "_smooch_session" setCookie vaultKey

lookupSession :: Ctxt -> Maybe SmoochSession
lookupSession ctxt = V.lookup (ctxt ^. sessionKey) (vault (fst $ ctxt ^. req))

setInSession :: Ctxt -> Text -> Text -> IO ()
setInSession ctxt k v =
  case lookupSession ctxt of
    Just (_, setSession) -> setSession k v
    Nothing -> error $ "setInSession: no session in vault"

getFromSession :: Ctxt -> Text -> IO (Maybe Text)
getFromSession ctxt k =
  case lookupSession ctxt of
    Just (getSession, _) -> getSession k
    Nothing -> error $ "getFromSession: no session in vault"

loginHandler :: Ctxt -> Text -> Text ->IO (Maybe Response)
loginHandler ctxt username password = do
  mUser <- authenticateUser ctxt username password
  case mUser of
    Just _user -> do
      setInSession ctxt "user" username
      mLoggedInUser <- getFromSession ctxt "user"
      case mLoggedInUser of
        Just user -> okText $ user <> " is logged in!"
        Nothing -> okText "you're logged in!!! (lie)"
    Nothing    -> errText "Your username or password was wrong :("

loggedInUserSplices :: Substitutions Ctxt
loggedInUserSplices =
  subs [("loggedInUser", textFill' userFill)]
  where userFill :: StateT Ctxt IO Text
        userFill = do
          ctxt <- get
          mUser <- liftIO $ getFromSession ctxt "user"
          case mUser of
            Just user -> return user
            Nothing -> return ""
