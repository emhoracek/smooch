{-# LANGUAGE OverloadedStrings #-}

module Web where

import           Control.Lens               ((^.))
import qualified Data.Configurator          as C
import           Data.Maybe                 (fromMaybe)
import           Data.Pool                  (createPool)
import           Data.Text                  (Text)
import qualified Data.Vault.Lazy            as V
import qualified Database.PostgreSQL.Simple as PG
import           Network.HTTP.Types.Method  (StdMethod (..))
import           Network.Wai                (Application, Response)
import           System.Environment         (lookupEnv)
import           Web.Fn
import           Web.Larceny                hiding (renderWith)

import           Ctxt
import           Session
import           Users.Controller
import           Dolls.Controller
import           Users.Model
import           Users.View
import           Dolls.View

initializer :: IO Ctxt
initializer = do
  env <- fromMaybe "devel" <$> lookupEnv "ENV"
  let env' = if env == "" then "devel" else env
  conf <- C.load [C.Required (env' <> ".cfg")]
  dbHost <- C.require conf "postgresql-simple.host"
  dbPort <- C.require conf "postgresql-simple.port"
  dbUser <- C.require conf "postgresql-simple.user"
  dbPass <- C.require conf "postgresql-simple.pass"
  dbName <- C.require conf "postgresql-simple.db"
  dbPool <- createPool (PG.connect (PG.ConnectInfo dbHost
                                                   dbPort
                                                   dbUser
                                                   dbPass
                                                   dbName))
            PG.close 1 60 20
  skipLoginUser <- C.lookup conf "skip-login-user"
  lib <- loadTemplates "templates" defaultOverrides
  vaultKey <- V.newKey
  let globalSubs = subs [("if", ifFill)]
  return (Ctxt defaultFnRequest vaultKey lib globalSubs skipLoginUser dbPool)

app :: IO Application
app = do
  ctxt <- initializer
  appBase ctxt

-- appBase is used with hspec-fn for testing
appBase :: Ctxt -> IO Application
appBase ctxt = do
  let key = ctxt ^. sessionKey
  sessionMid <- sessionMiddleware key
  return $ sessionMid (toWAI ctxt site)

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ end ==> indexHandler
             , method POST // path "login"
                           // param "username"
                           // param "password" !=> loginHandler
             , method POST // path "logout" !=> logoutHandler
             , method POST // path "dolls"
                           // path "upload"
                           // file "kissfile"
                           !=> fileUploadHandler
             , path "users" ==> usersRoutes
             , path "dolls" // path "upload" // param "link" !=> linkUploadHandler
             , path "static" // anything ==> staticServe "static"
             , anything ==> larcenyServe ]
    `fallthrough` notFoundText "Page not found."

indexHandler :: Ctxt -> IO (Maybe Response)
indexHandler ctxt = renderWith ctxt ["index"] (createUserErrorSplices <> loggedInUserSplices <> linkUploadSplices)

loginHandler :: Ctxt -> Text -> Text -> IO (Maybe Response)
loginHandler ctxt username password = do
  mUser <- authenticateUser ctxt username password
  case mUser of
    Just user -> do
      setLoggedInUser ctxt user
      redirect ("/users/" <> username)
    Nothing    -> errText "Your username or password was wrong :("

logoutHandler :: Ctxt -> IO (Maybe Response)
logoutHandler ctxt = do
    setLoggedOut ctxt
    redirect "/"
