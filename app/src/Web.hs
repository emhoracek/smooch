{-# LANGUAGE OverloadedStrings #-}

module Web where

import           Control.Lens               ((^.))
import qualified Data.Configurator          as C
import           Data.Monoid                ((<>))
import           Data.Pool                  (createPool)
import           Data.Text                  (Text)
import qualified Data.Vault.Lazy            as V
import qualified Database.PostgreSQL.Simple as PG
import           Network.HTTP.Types.Method  (StdMethod (..))
import           Network.Wai                (Application, Response)
import           System.Environment         (getEnv)
import           Web.Fn
import           Web.Larceny                hiding (renderWith)

import           Ctxt
import           Session
import           Sets.Controller
import           Users.Controller
import           Users.Model
import           Users.View

initializer :: IO Ctxt
initializer = do
  env <- getEnv "ENV"
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
  lib <- loadTemplates "templates" defaultOverrides
  vaultKey <- V.newKey
  let globalSubs = subs [("if", ifFill)]
  return (Ctxt defaultFnRequest vaultKey lib globalSubs dbPool)

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
             , path "upload" // method POST // file "kissfile" !=> uploadHandler
             , path "sets" // segment // end ==> setHandler
             , method POST // path "login"
                           // param "username"
                           // param "password" !=> loginHandler
             , path "users" ==> usersRoutes
             , path "static" // anything ==> staticServe "static" ]
    `fallthrough` notFoundText "Page not found."

indexHandler :: Ctxt -> IO (Maybe Response)
indexHandler ctxt = renderWith ctxt ["index"] (createUserErrorSplices <> loggedInUserSplices)

loginHandler :: Ctxt -> Text -> Text ->IO (Maybe Response)
loginHandler ctxt username password = do
  mUser <- authenticateUser ctxt username password
  case mUser of
    Just user -> do
      setLoggedInUser ctxt user
      okText $ userUsername user <> " is logged in!"
    Nothing    -> errText "Your username or password was wrong :("
