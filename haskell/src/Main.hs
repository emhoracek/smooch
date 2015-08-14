{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import qualified Web.Scotty as S

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse

import Text.Blaze.Html5 hiding (main, map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H hiding (main, map)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

-- OMG I HATE TEXT IN HASKELL
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

import KissSet
import Index
import Upload

blaze = S.html . renderHtml

main = scotty 3000 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")

  get "/" $
    blaze Index.render

  post "/upload" $ do
    fs <- files
    relDir <- liftIO $ runEitherT $ getRelDir fs 
    cels <- liftIO $ runEitherT $ processSet fs
    -- augh nesting, no sir I don't like it
    case relDir of
      Right dir -> case cels of 
                     Right x -> blaze $ KissSet.render dir (reverse x)
                     Left  e -> S.text $ LT.fromStrict e
      Left e -> S.text $ LT.fromStrict e

