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

import System.FilePath ((</>), takeBaseName, takeExtension)
import System.Directory
import System.Cmd
import System.Exit

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

import KissSet
import Index

import ParseCNF
import Kiss
import Shell
import Data.Aeson.Encode (encode) 

blaze = S.html . renderHtml

main = scotty 3000 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")

  get "/" $
    blaze Index.render

  post "/upload" $ do
    fs <- files
    -- augh nesting, no sir I don't like it
    relDir <- liftIO $ runEitherT $ getRelDir fs 
    cels <- liftIO $ runEitherT $ processSet fs
    case relDir of
      Right dir -> case cels of 
                     Right x -> blaze $ KissSet.render dir (reverse x)
                     Left  e -> S.text $ LT.fromStrict e
      Left e -> S.text $ LT.fromStrict e

processSet :: [S.File] -> EitherT T.Text IO [KissCell]
processSet files = do
  file <- getFile files
  let fileName = fst file
  let fileContents = snd file
  let staticDir = "static/sets/" <> takeBaseName fileName
  liftIO $ B.writeFile ("static/sets" </> fileName) fileContents
  liftIO $ createDirectoryIfMissing ("create parents" == "false") staticDir
  unzipFile fileName staticDir
  cnf <- getCNF staticDir
  let kissData = getKissData cnf
  let kissCels = getKissCels cnf
  -- just using first palette found for now
  let kissPalette = Prelude.head $ kPalettes kissData
  let json = "var kissJson = " <> encode kissData
  liftIO $ B.writeFile (staticDir <> "/setdata.js") json
  convertCels kissPalette (map celName kissCels) staticDir
  return kissCels

getFile :: [S.File] -> EitherT T.Text IO (String, B.ByteString)
getFile files = EitherT $ return $
  case files of 
    [(_, b)]  -> Right (BS.unpack (fileName b), fileContent b)
    otherwise -> Left "Please upload exactly one file."

getRelDir :: [S.File] -> EitherT T.Text IO FilePath
getRelDir files = do
  file <- getFile files
  let fileName = fst file
  return $ "sets/" ++ takeBaseName fileName

unzipFile :: FilePath -> FilePath -> EitherT T.Text IO ()
unzipFile name dir = EitherT $ do 
  exit <- system $ "lha -xw=" <> dir <> " static/sets/" <> name
  case exit of 
    ExitSuccess -> return $ Right ()
    otherwise   -> return $ Left "Unable to decompress LZH archive."

-- for now, only looks at first cnf listed
getCNF :: FilePath -> EitherT T.Text IO String
getCNF dir = do
  files <- liftIO $ getDirectoryContents dir
  let cnfs = filter (\x -> takeExtension x == ".cnf") files 
  case cnfs of
    (x:xs)    -> liftIO $ readFile $ dir </> x
    otherwise -> EitherT $ return $ Left "No configuration file found."


