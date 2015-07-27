{-# LANGUAGE OverloadedStrings #-}

module Main where

import ParseCNF
import CreateHTML
import Kiss
import Shell
import System.Environment (getArgs)
import Data.Aeson.Encode(encode)
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.ByteString.Char8 (unpack)
import Filesystem.Path (directory)
import System.Directory (canonicalizePath)

--fileToDir :: FilePath -> IO FilePath
fileToDir file = canonicalizePath $ show (directory file)

main :: IO()
main = do
  args <- getArgs
  cnf <- readFile $ head args
  let kissData = getKissData cnf
  let kissCels = getKissCels cnf
  let kissPalette = head $ kPalettes kissData
  let json = "var kissJson = " ++ unpack (toStrict $ encode kissData)
  let installDir = "/home/libby/dev/smooch"
  dataDir <- fileToDir cnf 
  writeFile (dataDir ++ "/kiss.js") json
  writeHTML kissCels installDir dataDir
  convertCels kissPalette kissCels dataDir
