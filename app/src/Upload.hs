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
import           System.FilePath            (takeExtension, (</>))
import           Control.Exception
import           Control.Logging            (log')
import           Control.Monad              (when, void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except

import           Data.Aeson                 (encode)

import           CelToPng
import           Kiss
import qualified ParseCel                   as PC
import           ParseCNF
import qualified ParseKCF                   as PK
import           Shell                      (unzipFile, lowercaseFiles)
import           Dolls.Model

processDoll :: String
            -> FilePath
            -> FilePath
            -> ExceptT Text IO (FilePath, [KissCel])
processDoll dollHash lzhPath newLzhPath = do
  liftIO $ copyFile lzhPath ("static/sets" </> newLzhPath)
  staticDir <- createDollDir ("static/sets" </> dollHash)
  void $ unzipFile newLzhPath staticDir
  log' "Unzipped file!"
  liftIO $ lowercaseFiles staticDir
  log' "Lowercased file names"
  cels <- createCels staticDir
  return (staticDir, cels)

staticDollDir :: FilePath -> String -> FilePath
staticDollDir userDir setName =  userDir <> "/" <> setName

createDollDir :: FilePath -> ExceptT Text IO FilePath
createDollDir staticDir = do
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

getCels :: Doll -> ExceptT Text IO (FilePath, [KissCel])
getCels doll =
  case dollError doll of
    Nothing -> do
      let loc = "static/sets/" ++ T.unpack (T.decodeUtf8 (dollHash doll))
      log' "About to get CNF"
      cnf <- getCNF loc
      log' "Got CNF"
      KissDoll _ celData _ <- getKissDoll cnf
      log' "Parsed CNF"
      celsWithOffsets <- readCels (nub celData) loc
      log' "Loaded cels"
      let realCelData = addOffsetsToCelData celsWithOffsets
      return (loc, realCelData)
    Just err -> throwE err

createCels :: FilePath -> ExceptT Text IO [KissCel]
createCels staticDir = do
  log' "About to get CNF"
  cnf <- getCNF staticDir
  log' "Got CNF"
  KissDoll cnfKissData celData kissPalettes <- getKissDoll cnf
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

readCels :: [CNFKissCel] -> String -> ExceptT Text IO [(CNFKissCel, (Int, Int))]
readCels cels base = mapM (readCel base) cels

readCel :: String -> CNFKissCel -> ExceptT Text IO (CNFKissCel, (Int, Int))
readCel base cnfCel= do
  let cel = cnfCelName cnfCel
      celFile = base <> "/" <> cel <> ".cel"
  celData <- liftIO $ BS.readFile celFile
  (celHeader, _) <- PC.parseCel celData
  return (cnfCel, getOffset celHeader)

addOffsetsToCelData :: [(CNFKissCel, (Int, Int))] -> [KissCel]
addOffsetsToCelData offsets =
  [ KissCel cnfCelMark cnfCelFix cnfCelName cnfCelPalette cnfCelSets cnfCelAlpha (Position xoff yoff)
     | (CNFKissCel{..}, (xoff, yoff)) <- offsets]

addCelsAndColorsToKissData :: CNFKissData -> Color -> Color -> [KissCel] -> KissData
addCelsAndColorsToKissData (CNFKissData m _ p ws _ sp fkiss) bgColor borderColor cels =
  KissData m borderColor bgColor p ws cels sp fkiss

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
