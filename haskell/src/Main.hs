{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Exception          (SomeException (..), catch)
import           Control.Lens
import           Control.Logging
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either
import qualified Data.ByteString            as BS
import           Data.List                  (lookup)
import           Data.Maybe                 (listToMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TD
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Parse
import           System.Environment
import           Web.Fn
import           Web.Fn.Extra.Heist

import           Upload

data Ctxt = Ctxt { _req   :: Request,
                   _heist :: FnHeistState Ctxt}

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

instance HeistContext Ctxt where
  heistLens = heist

initializer :: IO Ctxt
initializer = do
  hs' <- heistInit ["templates"] mempty
  let hs = case hs' of
             Left ers -> errorL' ("Heist failed to load templates: \n" <> T.intercalate "\n" (map T.pack ers))
             Right hs'' -> hs''
  return (Ctxt defaultRequest hs)

app :: IO Application
app = do
  ctxt <- initializer
  return $ toWAI ctxt site

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ end ==> indexHandler
             , path "upload" // method POST  ==> uploadHandler
             , anything ==> staticServer ]
    `fallthrough` notFoundText "Page not found."

staticServer :: Ctxt -> IO (Maybe Response)
staticServer ctxt =
   let f = T.intercalate "/" ("static" : pathInfo (_req ctxt)) in
   return $ Just $ responseFile status200 [] (T.unpack f) Nothing

indexHandler :: Ctxt -> IO (Maybe Response)
indexHandler ctxt = render ctxt "index"

uploadHandler :: Ctxt -> IO (Maybe Response)
uploadHandler ctxt = do
    ps <- parseRequestBody lbsBackEnd (_req ctxt)
    let fs = snd ps
    relDir <- liftIO $ runEitherT $ getRelDir fs
    cels <- liftIO $ runEitherT $ processSet fs
    -- augh nesting, no sir I don't like it
    case relDir of
      Right _ -> case cels of
                   Right _ -> okText "yay"
                   Left  e -> okText e
      Left e -> okText e

main :: IO ()
main = withStdoutLogging $ do
  port <- maybe 8000 read <$> lookupEnv "PORT"
  log' $ "Starting server on port " <> T.pack (show port) <> "..."
  app' <- app
  catch (run port app')
        (\(_ :: SomeException) ->
           log' "Shutting down...")
