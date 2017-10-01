{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Web where

import           Control.Monad.Trans.Either
import qualified Data.Configurator          as C
import           Data.Pool
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PG
import           Network.HTTP.Types.Method
import           Network.Wai
import           System.FilePath            (takeBaseName)
import           Web.Fn
import           Web.Larceny                hiding (renderWith)

import           Ctxt
import           Kiss
import           Upload
import           Users.Controller

initializer :: IO Ctxt
initializer = do
  conf <- C.load [C.Required "devel.cfg"]
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
  return (Ctxt defaultFnRequest lib mempty dbPool)

app :: IO Application
app = do
  ctxt <- initializer
  appBase ctxt

-- appBase is used with hspec-fn for testing
appBase :: Ctxt -> IO Application
appBase ctxt = return $ toWAI ctxt site

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ end ==> indexHandler
             , path "upload" // method POST // file "kissfile" !=> uploadHandler
             , path "sets" // segment // end ==> setHandler
             , path "users" ==> userRoutes
             , path "static" // anything ==> staticServe "static" ]
    `fallthrough` notFoundText "Page not found."

indexHandler :: Ctxt -> IO (Maybe Response)
indexHandler ctxt = renderWith ctxt ["index"] createUserErrorSplices

uploadHandler :: Ctxt -> File -> IO (Maybe Response)
uploadHandler ctxt (File name _ filePath') = do
  let staticDir = staticDirFromSetName (takeBaseName (T.unpack name))
  cels <- runEitherT $ processSet (T.unpack name, filePath')
  renderKissSet ctxt staticDir cels

setHandler :: Ctxt -> T.Text -> IO (Maybe Response)
setHandler ctxt setName = do
  let staticDir = staticDirFromSetName (T.unpack setName)
  cels <- runEitherT $ createCels staticDir
  renderKissSet ctxt staticDir cels

renderKissSet :: Ctxt -> String -> Either T.Text [KissCell] -> IO (Maybe Response)
renderKissSet ctxt staticDir cels =
  case cels of
    Right cs -> renderWith ctxt ["kissSet"] $ setSplices staticDir cs
    Left  e -> okText e

setSplices :: String -> [KissCell] -> Substitutions Ctxt
setSplices staticDir cs =
  subs [("set-listing", setListingSplice),
        ("base", textFill (T.pack staticDir)),
        ("celImages", celsSplice staticDir cs)]

setListingSplice :: Fill Ctxt
setListingSplice =
  mapSubs toSet ([0..9] :: [Int])
  where toSet n =
          subs [("set-number", (textFill . T.pack . show) n)]

celsSplice :: FilePath -> [KissCell] -> Fill Ctxt
celsSplice dir cels =
  mapSubs (celImageSplice dir) (reverse cels)

celImageSplice :: FilePath -> KissCell -> Substitutions Ctxt
celImageSplice dir cel =
  subs [("cel-name", textFill $ T.pack $ celName cel)
       ,("dir", textFill $ T.pack dir)]
