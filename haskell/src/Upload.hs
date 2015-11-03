{-# LANGUAGE OverloadedStrings #-}

module Upload where

import           Network.Wai.Parse

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as B
import qualified Data.Text                  as T

import           System.Directory
import           System.Exit
import           System.FilePath            (takeBaseName, takeExtension, (</>))

import           Control.Exception
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either
import           Data.Monoid                ((<>))

import           Data.Aeson.Encode          (encode)
import           Kiss
import           ParseCNF
import           Shell

processSet :: [File B.ByteString] -> EitherT T.Text IO [KissCell]
processSet files = do
  file <- getFile files
  let fName = fst file
  let fileContents = snd file
  let staticDir = "static/sets/" <> takeBaseName fName
  tryIO $ B.writeFile ("static/sets" </> fName) fileContents
  let createParents = True
  tryIO $ createDirectoryIfMissing createParents staticDir
  unzipFile fName staticDir
  cnf <- getCNF staticDir
  kissData <- getKissData cnf
  let json = "var kissJson = " <> encode kissData
  kissCels <- getKissCels cnf
  -- just using first palette found for now
  kissPalette <- getKissPalette kissData
  tryIO $ B.writeFile (staticDir <> "/setdata.js") json
  convertCels kissPalette (map celName kissCels) staticDir
  return kissCels

tryIO :: IO () -> EitherT T.Text IO ()
tryIO f = do
  result <-liftIO (try f :: IO (Either IOException ()))
  case result of
    Right () -> return ()
    Left ex  -> EitherT $ return $ Left (T.pack $ show ex)

getFile :: [File B.ByteString] -> EitherT T.Text IO (String, B.ByteString)
getFile files = EitherT $ return $
  case files of
    [(_, b)]   -> Right (BS.unpack (fileName b), fileContent b)
    _otherwise -> Left "Please upload exactly one file."

getRelDir :: [File B.ByteString] -> EitherT T.Text IO FilePath
getRelDir files = do
  file <- getFile files
  let fName = fst file
  return $ "sets/" ++ takeBaseName fName

-- for now, only looks at first cnf listed
getCNF :: FilePath -> EitherT T.Text IO String
getCNF dir = do
  files <- liftIO $ getDirectoryContents dir
  let cnfs = filter (\x -> takeExtension x == ".cnf") files
  case cnfs of
    (x:_xs)     -> liftIO $ readFile $ dir </> x
    _otherwise -> EitherT $ return $ Left "No configuration file found."
