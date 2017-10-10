{-# LANGUAGE OverloadedStrings #-}

module Session where

import           Control.Lens            ((^.))
import           Data.Text               (Text)
import qualified Data.Vault.Lazy         as V
import           Network.Wai             (Middleware, vault)
import           Network.Wai.Session     (genSessionId, withSession)
import           Network.Wai.Session.Map (mapStore)
import           Web.Cookie              (def)

import           Ctxt

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
    Nothing -> error "setInSession: no session in vault"

getFromSession :: Ctxt -> Text -> IO (Maybe Text)
getFromSession ctxt k =
  case lookupSession ctxt of
    Just (getSession, _) -> getSession k
    Nothing -> error "getFromSession: no session in vault"
