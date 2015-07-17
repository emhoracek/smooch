module Main where

import ParseCNF
import CreateHTML
import Kiss
import Shell
import System.Environment (getArgs)
import Data.Aeson.Encode(encode)
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.ByteString.Char8 (unpack)

main :: IO()
main = do
  args <- getArgs
  cnf <- readFile $ head args
  let kissData = getKissData cnf
  let kissCels = getKissCels cnf
  let kissPalette = head $ kPalettes kissData
  let json = "var kissJson = " ++ unpack (toStrict $ encode kissData)
  header <- readFile "header.html"
  footer <- readFile "footer.html"
  let html = header ++ "\n" ++ htmlCelImages kissCels ++ "\n" ++ footer 
  writeFile "kiss.js" json
  writeFile "kiss.html" html
  convertCels kissPalette kissCels "tests/samples"
