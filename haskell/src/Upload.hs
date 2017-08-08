{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Upload where

import qualified Data.ByteString.Lazy       as B
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

import           System.Directory
import           System.FilePath            (takeBaseName, takeExtension, (</>))

import           Control.Exception
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either
import           Data.Monoid                ((<>))

import           Control.Logging            (log')

import           Data.Aeson                 (encode)
import           Kiss
import           ParseCNF
import           Shell

processSet :: (FilePath, FilePath) -> EitherT Text IO [KissCell]
processSet (fName, filePath) = do
  tryIO $ copyFile filePath ("static/sets" </> fName)
  staticDir <- createSetDir (takeBaseName fName)
  unzipFile fName staticDir
  createCels staticDir

staticDirFromSetName :: String -> FilePath
staticDirFromSetName setName = "static/sets/" <> setName

createSetDir :: String -> EitherT Text IO FilePath
createSetDir setName = do
  let staticDir = "static/sets/" <> setName
  exists <- liftIO $ doesDirectoryExist staticDir
  when exists $ tryIO $ removeDirectoryRecursive staticDir
  tryIO $ createDirectory staticDir
  log' $ "Created static directory: " <> T.pack staticDir
  return staticDir

deleteCels :: FilePath -> IO ()
deleteCels staticDir = do
  allFiles <- listDirectory staticDir
  let cels = filter (\f -> takeExtension f == "cel") allFiles
  mapM_ removeFile cels

createCels :: FilePath -> EitherT Text IO [KissCell]
createCels staticDir = do
  cnf <- getCNF staticDir
  KissSet kissData celData kissPalette <- getKissSet cnf
  log' $ "Parsed CNF"
  celsWithOffsets <- convertCels kissPalette (map cnfCelName celData) staticDir
  log' $ "Converted cels"
  let realCelData = addOffsetsToCelData celsWithOffsets celData
  bgColor <- colorByIndex (kBorder kissData) (staticDir </> kissPalette)
  let json = "var kissJson = " <> encode kissData <> ";\n" <>
             "var celJson = " <> encode realCelData <> ";\n" <>
             "var borderColor = \"" <>
             B.fromStrict (T.encodeUtf8 (T.pack bgColor)) <> "\";\n"
  log' $ T.pack $ show json
  tryIO $ B.writeFile (staticDir <> "/setdata.js") json
  log' $ "Wrote JSON"
  return realCelData

addOffsetsToCelData :: [(String, (Int, Int))] -> [CNFKissCell] ->
                       [KissCell]
addOffsetsToCelData offsets cells =
  [ KissCell cnfCelFix cnfCelName cnfCelPalette cnfCelSets cnfCelAlpha (Position xoff yoff)
     | CNFKissCell{..} <- cells, offset@(_, (xoff, yoff)) <- offsets, cnfCelName == fst offset]

tryIO :: IO () -> EitherT Text IO ()
tryIO f = do
  result <-liftIO (try f :: IO (Either IOException ()))
  case result of
    Right () -> return ()
    Left ex  -> EitherT $ return $ Left (T.pack $ show ex)

-- for now, only looks at first cnf listed
getCNF :: FilePath -> EitherT Text IO String
getCNF dir = do
  files <- liftIO $ getDirectoryContents dir
  let cnfs = filter (\x -> takeExtension x == ".cnf") files
  case cnfs of
    (x:_xs)     -> liftIO $ readFile $ dir </> x
    _otherwise -> EitherT $ return $ Left "No configuration file found."
