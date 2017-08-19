{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Upload where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Text.ICU.Convert

import           System.Directory
import           System.FilePath            (takeBaseName, takeExtension, (</>))

import           Control.Exception
import           Control.Logging            (log')
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either
import           Data.Monoid                ((<>))
import           Data.Bifunctor             (first)

import           Data.Aeson                 (encode)

import           Kiss
import           ParseCNF
import           Shell

processSet :: (FilePath, FilePath) -> EitherT Text IO [KissCell]
processSet (fName, filePath) = do
  tryIO $ copyFile filePath ("static/sets" </> fName)
  staticDir <- createSetDir (takeBaseName fName)
  unzipFile fName staticDir
  log' "Unzipped file!"
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
  log' "About to get CNF"
  cnf <- getCNF staticDir
  log' "Got CNF"
  KissSet cnfKissData celData kissPalettes <- getKissSet cnf
  log' "Parsed CNF"
  celsWithOffsets <- convertCels kissPalettes celData staticDir
  log' "Converted cels"
  let realCelData = addOffsetsToCelData celsWithOffsets celData
  log' "Added offsets"
  defPalette <- defaultPalette kissPalettes
  log' "Got default palette"
  bgColor <- colorByIndex 0 (staticDir </> defPalette)
  log' "Got bg color"
  borderColor <- colorByIndex (cnfkBorder cnfKissData) (staticDir </> defPalette)
  log' "Got border color"
  let kissData = addCelsAndColorsToKissData cnfKissData bgColor borderColor realCelData
  log' "Added cels and colors to kiss data"
  let json = "var kissJson = " <> encode kissData <> ";\n"
  tryIO $ LBS.writeFile (staticDir <> "/setdata.js") json
  log' "Wrote JSON"
  return realCelData

addOffsetsToCelData :: [(String, (Int, Int))] -> [CNFKissCell] ->
                       [KissCell]
addOffsetsToCelData offsets cells =
  [ KissCell cnfCelFix cnfCelName cnfCelPalette cnfCelSets cnfCelAlpha (Position xoff yoff)
     | CNFKissCell{..} <- cells, offset@(_, (xoff, yoff)) <- offsets, cnfCelName == fst offset]

addCelsAndColorsToKissData :: CNFKissData -> Color -> Color -> [KissCell] -> KissData
addCelsAndColorsToKissData (CNFKissData m _ p ws o) bgColor borderColor cels =
  KissData m borderColor bgColor p ws o cels

tryIO :: IO a -> EitherT Text IO a
tryIO m = EitherT $ first showIOException <$> try m
  where
    showIOException = T.pack . show :: IOException -> Text

-- for now, only looks at first cnf listed
getCNF :: FilePath -> EitherT Text IO String
getCNF dir = do
  files <- tryIO $ getDirectoryContents dir
  let cnfs = filter (\x -> takeExtension x == ".cnf") files
  case cnfs of
    (f:_fs) -> tryIO $ readUtf8OrShiftJis (dir </> f)
    _ -> left "No configuration file found."

readUtf8OrShiftJis :: String -> IO String
readUtf8OrShiftJis fp = do
  fileBS <- BS.readFile fp
  fileText <- catch (return $! T.decodeUtf8 fileBS)
                   (\(_ :: SomeException) -> decodeShiftJiS fileBS)
  return $ T.unpack fileText
  where decodeShiftJiS bs = do
          convert <- toUnicode <$> open "SHIFT_JIS" Nothing
          return $ convert bs
