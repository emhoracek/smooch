{-# LANGUAGE OverloadedStrings #-}

module Shell where

import Kiss
import qualified Data.Text as Text
import Turtle hiding (FilePath)
import System.Process (readProcessWithExitCode)
import System.Directory

-- Gets the transparency color from a kcf palette.
transColor :: String -> IO String
transColor paletteLoc = do
  (_, x, _) <- readProcessWithExitCode "cel2pnm" ["-t", paletteLoc] ""
  return x

-- Convert a whole list of cels given a palette. Put the files in target directory.
convertCels :: String -> [KissCell] -> String -> IO ()
convertCels palString cels dir = do
  let pal = Text.pack palString
  let name c = Text.pack $ celName c
  let out = Text.pack dir
  trans <- transColor palString
  mapM_ (\ cel -> convertCel pal (name cel) (Text.pack trans) out) cels

-- Convert cel to pnm, pnm to png, delete pnm
convertCel :: Text -> Text -> Text -> Text -> IO ()
convertCel palette cel bg out = do
  let celFile = cel <> ".cel"
  let pngFile = cel <> ".png"
  shell ("cel2pnm " <> palette <> " " <> cel <> " pnm") empty
  shell ("pnmtopng -transparent " <> bg <> " pnm > " <> out <> "/" <> pngFile) empty
  rm "pnm"

