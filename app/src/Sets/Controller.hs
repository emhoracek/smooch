{-# LANGUAGE OverloadedStrings #-}

module Sets.Controller where

import           Control.Monad.Trans.Either (runEitherT)
import qualified Data.Text                  as T
import           Network.Wai                (Response)
import           System.FilePath            (takeBaseName)
import           Web.Fn

import           Ctxt
import           Kiss
import           Sets.View
import           Upload

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
