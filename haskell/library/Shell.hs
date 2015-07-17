{-# LANGUAGE OverloadedStrings #-}

module Shell where

import Kiss
import qualified Data.Text as Text
import Turtle

{-- Just keeping this around because I want to do a  blog post on why it doesn't work
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

convertCels :: String -> [KissCell] -> IO ()
convertCels palString cels = do
  let palette = Text.pack palString
  shell ("./cel2pnm -t " <> palette <> " > bg") empty
  bg <- readFile "bg"
  mapM_ (\ cel -> convertCel palette (name cel) (Text.pack bg)) cels
  where name c = Text.pack $ celName c

-- Convert cell to pnm, pnm to png, delete pnm
convertCel :: Text -> Text -> Text -> IO ()
convertCel palette cel bg = do
  let celFile = cel <> ".cel"
  let pngFile = cel <> ".png"
  shell ("./cel2pnm " <> celFile <> " " <> palette <> " pnm") empty
  shell ("pnmtopng -transparent " <> bg <> " pnm > " <> pngFile) empty
  rm "pnm"

