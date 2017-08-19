{-# LANGUAGE OverloadedStrings #-}

module Shell where

import           Control.Monad.Trans.Either
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           System.Exit                (ExitCode (..))
import           System.IO                  (hGetContents)
import           System.Process

import           Kiss
import           ParseCNF

-- Gets the transparency color from a kcf palette.
transColor :: PaletteFilename -> EitherT T.Text IO String
transColor paletteLoc = EitherT $ do
  (_, colorOut, err, ph)  <- createProcess (shell $ "cel2pnm -t " <> paletteLoc) { std_out = CreatePipe, std_err = CreatePipe }
  result <- waitForProcess ph
  errMsg <- case err of
              Just x  -> hGetContents x
              Nothing -> return "no error message"
  color <- case colorOut of
              Just x  -> hGetContents x
              Nothing -> return "no color"
  case result of
    ExitSuccess   -> return $ Right color
    ExitFailure n -> return $ Left $ T.pack ("Error while finding transparency color. Exit code: " <> show n <> ". Error: " <> errMsg)


-- Gets an indexed color from a kcf palette.
colorByIndex :: Int -> PaletteFilename -> EitherT T.Text IO String
colorByIndex colorNum paletteLoc = EitherT $ do
  (_,colorOut, err, ph)  <- createProcess (shell $ "cel2pnm -c " <> show colorNum <> " " <> paletteLoc) { std_out = CreatePipe, std_err = CreatePipe }
  result <- waitForProcess ph
  errMsg <- case err of
              Just x  -> hGetContents x
              Nothing -> return "no error message"
  color <- case colorOut of
              Just x -> hGetContents x
              Nothing -> return "no color"
  case result of
    ExitSuccess   -> return $ Right color
    ExitFailure n -> return $ Left $ T.pack ("Error while finding background color. Exit code: " <> show n <> ". Error: " <> errMsg)

-- Convert a whole list of cels given a palette. Put the files in target directory. Return list of cels with offset information
convertCels :: Palettes
            -> [CNFKissCell]
            -> String
            -> EitherT T.Text IO [ (String, (Int, Int)) ]
convertCels pals cels base = do
  mapM (\(CNFKissCell _ name pal _ _) -> convertCel pals pal name base) cels

-- Convert cel to pnm, pnm to png, delete pnm
convertCel :: Palettes
           -> Int
           -> CelFilename
           -> String
           -> EitherT T.Text IO (String, (Int, Int))
convertCel palettes palNum cel base = do
  pal <- lookupPalette palNum palettes
  trans <- transColor $ base ++ "/" ++ pal
  let celFile = base <> "/" <> cel <> ".cel"
  let pngFile = base <> "/" <> cel <> ".png"
  let paletteFile = base <> "/" <> pal
  offsets <- convertAndGetOffsets paletteFile celFile
  runProgram ("converting to png " <> pngFile)
             ("pnmtopng " <> " -transparent " <> trans <> " pnm > " <> pngFile)
  runProgram "removing temp file" "rm pnm"
  return (cel, offsets)

-- Gets the transparency color from a kcf palette.
convertAndGetOffsets :: PaletteFilename -> CelFilename -> EitherT T.Text IO (Int, Int)
convertAndGetOffsets paletteFile celFile = EitherT $ do
  (_,offsetOut, err, ph)  <- createProcess (shell ("cel2pnm -o " <> paletteFile <> " " <> celFile <> " pnm")) { std_out = CreatePipe, std_err = CreatePipe }
  result <- waitForProcess ph
  errMsg <- case err of
              Just x  -> hGetContents x
              Nothing -> return "no error message"
  offsets <- case offsetOut of
              Just x -> hGetContents x
              Nothing -> return "no color"
  case result of
    ExitSuccess   -> return $
                     case words offsets of
                       [] ->  Left "no offset data returned"
                       (x: y: _ ) -> Right (read x, read y)
                       _ -> Left "Bad offset output "
    ExitFailure n -> return $ Left $ T.pack ("Error while converting cel " <> celFile <> ". Exit code: " <> show n <> ". Error: " <> errMsg)

runProgram :: String -> String -> EitherT T.Text IO ()
runProgram name process = EitherT $ do
  (_, _, err, ph) <- createProcess (shell process) { std_err = CreatePipe }
  result <- waitForProcess ph
  errMsg <- case err of
              Just x  -> hGetContents x
              Nothing -> return "no error message"
  case result of
    ExitSuccess   -> return $ Right ()
    ExitFailure n -> return $ Left $ T.pack ("Error while " <> name <> ". Exit code: " <> show n <> ". Error: " <> errMsg)

unzipFile :: FilePath -> FilePath -> EitherT T.Text IO ()
unzipFile name dir = runProgram ("decompressing archive " <> name) ("lha -xw=" <> dir <> " static/sets/" <> name)
