{-# LANGUAGE OverloadedStrings #-}

module Main where

import ParseCNF
import Kiss
import CreateHTML
import qualified Data.Text as Text
import qualified Turtle as T
import Data.Char (toLower)
import qualified Control.Foldl as F
import System.Environment (getArgs)

import Data.Aeson.Encode(encode)
import Data.Text.Lazy as L (toStrict)
import Data.Text.Lazy.Encoding as E
import Data.Text.IO as DT (writeFile) 
{--
mvIt :: T.Shell (IO ())
mvIt = do
  files <- getFiles
  lower <- loweredFiles
  return $ T.cp files lower

-- THIS IS WRONG OMG
lowercaseFilename :: T.FilePath -> T.FilePath -- ???
lowercaseFilename x = T.fromString $ map toLower $ show x

loweredFiles :: T.Shell T.FilePath
loweredFiles = do
    files <- 
    return $ lowercaseFilename files

getFiles :: T.Shell T.FilePath
getFiles = T.ls "examples"
--}

convertCells palette cells = do
  T.shell ("./cel2pnm -t " T.<> palette T.<> " > bg") T.empty
  bg <- readFile "bg"
  mapM_ (\ cell -> convertCell palette (name cell) (Text.pack bg) "out") cells
  where name c = Text.pack $ celName c

-- Convert cell to pnm, pnm to png, delete pnm
convertCell palette cell bg out = do
  let cellFile = cell T.<> ".cel"
  T.shell ("./cel2pnm " T.<> cell T.<> " " T.<> palette T.<> " pnm") T.empty
  T.shell ("pnmtopng -transparent " T.<> bg T.<> " pnm > " T.<> out) T.empty
  T.rm "pnm"

main = do
  args <- getArgs
  cnf <- readFile $ head args
  let kissData = getKissData cnf
  let kissCells = getKissCells cnf
  let kissPalette = head $ kPalettes kissData
  let json = "var kissJson = " T.<> (decodeUtf8 (encode kissData))
  head <- readFile "header.html"
  footer <- readFile "footer.html"
  let html = head ++ "\n" ++ htmlCellImages kissCells ++ "\n" ++ footer 
  DT.writeFile "kiss.js" (toStrict json)
  Prelude.writeFile "kiss.html" html

