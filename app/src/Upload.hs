{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Upload where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.List                  (nub, sort)
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
import           Control.Monad.Trans.Except
import           Data.Monoid                ((<>))

import           Data.Aeson                 (encode)

import           CelToPng
import           Kiss
import qualified ParseCel                   as PC
import           ParseCNF
import qualified ParseKCF                   as PK
import           Shell                      (unzipFile, lowercaseFiles)

processSet :: Text
           -> (FilePath, FilePath)
           -> ExceptT Text IO (FilePath, [KissCel])
processSet username (fName, filePath) = do
  liftIO $ copyFile filePath ("static/sets" </> fName)
  userDir <- createUserDir username
  staticDir <- createSetDir userDir (takeBaseName fName)
  unzipFile fName staticDir
  log' "Unzipped file!"
  liftIO $ lowercaseFiles staticDir
  log' "Lowercased file names"
  cels <- createCels staticDir
  return (staticDir, cels)

staticUserDir :: Text -> FilePath
staticUserDir username = "static/sets/" <> T.unpack username

createUserDir :: Text -> ExceptT Text IO FilePath
createUserDir username = do
  let staticDir = staticUserDir username
  liftIO $ createDirectoryIfMissing True staticDir
  log' $ "Created static user sets directory if missing: " <> T.pack staticDir
  return staticDir

staticSetDir :: FilePath -> String -> FilePath
staticSetDir userDir setName =  userDir <> "/" <> setName

createSetDir :: FilePath -> String -> ExceptT Text IO FilePath
createSetDir userDir setName = do
  let staticDir = userDir <> "/" <> setName
  exists <- liftIO $ doesDirectoryExist staticDir
  when exists $ liftIO $ removeDirectoryRecursive staticDir
  liftIO $ createDirectory staticDir
  log' $ "Created static directory: " <> T.pack staticDir
  return staticDir

deleteCels :: FilePath -> IO ()
deleteCels staticDir = do
  allFiles <- listDirectory staticDir
  let cels = filter (\f -> takeExtension f == "cel") allFiles
  mapM_ removeFile cels

createCels :: FilePath -> ExceptT Text IO [KissCel]
createCels staticDir = do
  log' "About to get CNF"
  cnf <- getCNF staticDir
  log' "Got CNF"
  KissSet cnfKissData celData kissPalettes <- getKissSet cnf
  log' "Parsed CNF"
  celsWithOffsets <- convertCels kissPalettes (nub celData) staticDir
  log' "Converted cels"
  let realCelData = addOffsetsToCelData celsWithOffsets
  log' "Added offsets"
  defPalette <- defaultPalette kissPalettes
  log' "Got default palette"
  palData <- liftIO $ BS.readFile (staticDir </> defPalette)
  log' "Got default palette data"
  palEntries <- PK.parseKCF palData
  log' "Got default palette entries"
  let bgColor = PK.colorByIndex 0 palEntries
  log' "Got bg color"
  let borderColor = PK.colorByIndex (cnfkBorder cnfKissData) palEntries
  log' "Got border color"
  let kissData = addCelsAndColorsToKissData cnfKissData bgColor borderColor realCelData
  log' "Added cels and colors to kiss data"
  let json = "var kissJson = " <> encode kissData <> ";\n"
  liftIO $ LBS.writeFile (staticDir <> "/setdata.js") json
  log' "Wrote JSON"
  return realCelData

convertCels :: Palettes -> [CNFKissCel] -> String -> ExceptT Text IO [(CNFKissCel, (Int, Int))]
convertCels pals cels base = do
          mapM (convertCel pals base) cels

convertCel :: Palettes -> String -> CNFKissCel -> ExceptT Text IO (CNFKissCel, (Int, Int))
convertCel palettes base cnfCel= do
  let cel = cnfCelName cnfCel
      palNum = cnfCelPalette cnfCel
      celFile = base <> "/" <> cel <> ".cel"
      palDir = base <> "/palette" <> show palNum
      pngFile = palDir <> "/" <> cel <> ".png"
  pal <- lookupPalette palNum palettes
  paletteEntries <- liftIO $ BS.readFile (base <> "/" <> pal)
  palData <- PK.parseKCF paletteEntries
  celData <- liftIO $ BS.readFile celFile
  (celHeader, celPixels) <- PC.parseCel celData
  liftIO $ createDirectoryIfMissing True palDir
  liftIO $ celToPng pngFile palData celHeader celPixels
  return (cnfCel, getOffset celHeader)

getOffset :: PC.CelHeader -> (Int, Int)
getOffset celHeader =
  let xOffset = fromIntegral $ PC.celXoffset celHeader
      yOffset = fromIntegral $ PC.celYoffset celHeader in
    (xOffset, yOffset)

addOffsetsToCelData :: [(CNFKissCel, (Int, Int))] -> [KissCel]
addOffsetsToCelData offsets =
  [ KissCel cnfCelMark cnfCelFix cnfCelName cnfCelPalette cnfCelSets cnfCelAlpha (Position xoff yoff)
     | (CNFKissCel{..}, (xoff, yoff)) <- offsets]

addCelsAndColorsToKissData :: CNFKissData -> Color -> Color -> [KissCel] -> KissData
addCelsAndColorsToKissData (CNFKissData m _ p ws _ sp) bgColor borderColor cels =
  KissData m borderColor bgColor p ws cels sp

-- for now, only looks at first cnf listed
getCNF :: FilePath -> ExceptT Text IO String
getCNF dir = do
  files <- liftIO $ getDirectoryContents dir
  let cnfs = sort $ filter (\x -> takeExtension x == ".cnf") files
  case cnfs of
    (f:_fs) -> liftIO $ readUtf8OrShiftJis (dir </> f)
    _ -> throwE "No configuration file found."

readUtf8OrShiftJis :: String -> IO String
readUtf8OrShiftJis fp = do
  fileBS <- BS.readFile fp
  fileText <- catch (return $! T.decodeUtf8 fileBS)
                   (\(_ :: SomeException) -> decodeShiftJiS fileBS)
  return $ T.unpack fileText
  where decodeShiftJiS bs = do
          convert <- toUnicode <$> open "SHIFT_JIS" Nothing
          return $ convert bs
