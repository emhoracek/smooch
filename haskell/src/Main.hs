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

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import System.FilePath ((</>), takeBaseName, takeExtension)
import System.Directory
import System.Cmd
import System.Exit

import Control.Monad.IO.Class (liftIO)

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
    let file = case fs of 
                [(_, b)]  -> (BS.unpack (fileName b), fileContent b)
                otherwise -> error "Must upload exactly one file."
    let staticDir = "static/sets/" ++ takeBaseName (fst file)
    let relDir = "sets/" ++ takeBaseName (fst file)
    liftIO $ B.writeFile ("static/sets" </> fst file) (snd file)
    exit <- liftIO $ unzipFile (fst file) staticDir
    cels <- liftIO $ convertSet (fst file) staticDir
    blaze $ KissSet.render relDir (reverse cels)

unzipFile file dir = do 
  let base = takeBaseName file
  createDirectory dir
  exit <- system $ "lha -xw=" ++ dir ++ " static/sets/" ++ file
  case exit of 
    ExitSuccess -> return "yay!"
    otherwise   -> return "boo!"

-- for now, only looks at first cnf listed
convertSet file dir = do
  let base = takeBaseName file
  files <- getDirectoryContents dir
  let cnfs = filter (\x -> takeExtension x == ".cnf") files 
  cnf <- readFile $ dir </> Prelude.head cnfs
  let kissData = getKissData cnf
  print kissData
  let kissCels = getKissCels cnf
  let kissPalette = Prelude.head $ kPalettes kissData
  let json = "var kissJson = " ++ BS.unpack (B.toStrict $ encode kissData)
  writeFile (dir ++ "/setdata.js") json
  convertCels kissPalette (map celName kissCels) dir
  return kissCels
