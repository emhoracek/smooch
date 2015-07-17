{-# LANGUAGE OverloadedStrings #-}

module Shell where

import Kiss
import qualified Data.Text as Text
import Turtle
import System.Process hiding (shell)


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

transColor :: String -> IO String
transColor paletteLoc = do
  (_, x, _) <- readProcessWithExitCode "cel2pnm" ["-t", paletteLoc] ""
  return x


-- Convert a whole list of cells given a palette. Put the files in target directory.
convertCels :: String -> [KissCell] -> String -> IO ()
convertCels palString cels out = do
  trans <- transColor palString
  let name c = Text.pack $ celName c
  let dir = Text.pack out
  mapM_ (\ cel -> convertCel (Text.pack palString) (name cel) (Text.pack trans) dir) cels

-- Convert cell to pnm, pnm to png, delete pnm
convertCel :: Text -> Text -> Text -> Text -> IO ()
convertCel palette cel bg out = do
  let celFile = cel <> ".cel"
  let pngFile = cel <> ".png"
  shell ("cel2pnm " <> celFile <> " " <> palette <> " pnm") empty
  shell ("pnmtopng -transparent " <> bg <> " pnm > " <> out <> "/" <> pngFile) empty
  rm "pnm"

