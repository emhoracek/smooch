{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Exception          (SomeException (..), catch)
import           Control.Lens
import           Control.Logging
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Heist                      (( ## ))
import qualified Heist.Interpreted          as H
import           Network.HTTP.Types.Method
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Environment
import           System.FilePath            ((</>))
import qualified Text.XmlHtml               as X
import           Web.Fn
import           Web.Fn.Extra.Heist

import           Kiss
import           Upload

data Ctxt = Ctxt { _req   :: FnRequest,
                   _heist :: FnHeistState Ctxt}

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

instance HeistContext Ctxt where
  getHeist = _heist

initializer :: IO Ctxt
initializer = do
  hs' <- heistInit ["templates"] mempty mempty
  let hs = case hs' of
             Left ers -> errorL' ("Heist failed to load templates: \n" <> T.intercalate "\n" (map T.pack ers))
             Right hs'' -> hs''
  return (Ctxt defaultFnRequest hs)

app :: IO Application
app = do
  ctxt <- initializer
  return $ toWAI ctxt site

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ end ==> indexHandler
             , path "upload" // method POST // file "kissfile" !=> uploadHandler
             , anything ==> staticServe "static" ]
    `fallthrough` notFoundText "Page not found."

indexHandler :: Ctxt -> IO (Maybe Response)
indexHandler ctxt = render ctxt "index"

uploadHandler :: Ctxt -> File -> IO (Maybe Response)
uploadHandler ctxt (File name _ contents) = do
  relDir <- liftIO $ runEitherT $ getRelDir (T.unpack name, contents)
  cels <- liftIO $ runEitherT $ processSet (T.unpack name, contents)
  case relDir of
    Right d -> case cels of
      Right cs -> renderWithSplices ctxt "kissSet" $ do
                   tag' "set-listing" setListingSplice
                   "base" ## H.textSplice (T.pack d)
                   tag' "celImages" $ celsSplice d cs
      Left  e -> okText e
    Left e -> okText e

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
  [X.Element "img" [("src", T.pack (dir </> celName cel <> ".png")),
                    ("id", T.pack (celName cel)) ] []]

main :: IO ()
main = withStdoutLogging $ do
  port <- maybe 8000 read <$> lookupEnv "PORT"
  log' $ "Starting server on port " <> T.pack (show port) <> "..."
  app' <- app
  catch (run port app')
        (\(_ :: SomeException) ->
           log' "Shutting down...")
