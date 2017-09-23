{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Web where

import           Control.Lens
import           Control.Logging
import           Control.Monad.Trans.Either
import qualified Data.Configurator          as C
import           Data.Map.Syntax            (MapSyntaxM, ( ## ))
import           Data.Monoid                ((<>))
import           Data.Pool
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Heist.Interpreted          as H
import           Network.HTTP.Types.Method
import           Network.Wai
import           System.FilePath            (takeBaseName)
import           System.FilePath            ((</>))
import qualified Text.XmlHtml               as X
import           Web.Fn
import           Web.Fn.Extra.Heist

import           Kiss
import           Upload

data Ctxt = Ctxt { _req   :: FnRequest,
                   _heist :: FnHeistState Ctxt,
                   _pool  :: Pool PG.Connection }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

instance HeistContext Ctxt where
  getHeist = _heist

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
  hs' <- heistInit ["templates"] mempty mempty
  let hs = case hs' of
             Left ers -> errorL' ("Heist failed to load templates: \n" <> T.intercalate "\n" (map T.pack ers))
             Right hs'' -> hs''
  return (Ctxt defaultFnRequest hs dbPool)

app :: IO Application
app = do
  ctxt <- initializer
  return $ toWAI ctxt site

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ end ==> indexHandler
             , path "upload" // method POST // file "kissfile" !=> uploadHandler
             , path "sets" // segment // end ==> setHandler
             , path "static" // anything ==> staticServe "static" ]
    `fallthrough` notFoundText "Page not found."

indexHandler :: Ctxt -> IO (Maybe Response)
indexHandler ctxt = render ctxt "index"

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
    Right cs -> renderWithSplices ctxt "kissSet" $ setSplices staticDir cs
    Left  e -> okText e

setSplices :: String -> [KissCell] -> MapSyntaxM T.Text (FnSplice Ctxt) ()
setSplices staticDir cs = do
  tag' "set-listing" $ setListingSplice
  "base" ## H.textSplice (T.pack staticDir)
  tag' "celImages" $ celsSplice staticDir cs

setListingSplice :: Ctxt -> X.Node -> FnSplice Ctxt
setListingSplice _ _ =
  H.mapSplices toSet (map (T.pack . show) ([0..9] :: [Int]))
  where toSet n =
          return [X.Element "li" []
                   [X.Element "a" [("class", "set")] [ X.TextNode n ]]]


celsSplice :: FilePath -> [KissCell] -> Ctxt -> X.Node -> FnSplice Ctxt
celsSplice dir cels _ _ = H.mapSplices (celImageSplice dir ) (reverse cels)

celImageSplice :: FilePath -> KissCell -> FnSplice Ctxt
celImageSplice dir cel = return
  [X.Element "img" [("src", T.pack ("/" <> dir </> celName cel <> ".png")),
                    ("id", T.pack (celName cel)) ] []]
