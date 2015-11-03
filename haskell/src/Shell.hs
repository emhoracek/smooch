{-# LANGUAGE OverloadedStrings #-}

module Shell where

import           Control.Monad.Trans.Either
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Kiss
import           System.Directory
import           System.Exit
import           System.IO                  (hGetContents)
import           System.Process

-- Gets the transparency color from a kcf palette.
transColor :: String -> EitherT T.Text IO String
transColor paletteLoc = EitherT $ do
  (_,colorOut, err, ph)  <- createProcess (shell $ "cel2pnm -t " <> paletteLoc) { std_out = CreatePipe, std_err = CreatePipe }
  result <- waitForProcess ph
  errMsg <- case err of
              Just x  -> hGetContents x
              Nothing -> return "no error message"
  color <- case colorOut of
              Just x -> hGetContents x
              Nothing -> return "no color"
  case result of
    ExitSuccess   -> return $ Right color
    ExitFailure n -> return $ Left $ T.pack ("Error while finding transparency color. Exit code: " <> show n <> ". Error: " <> errMsg)

-- Convert a whole list of cels given a palette. Put the files in target directory.
convertCels :: String -> [String] -> String -> EitherT T.Text IO ()
convertCels pal cels base = do
  trans <- transColor $ base ++ "/" ++ pal
  mapM_ (\ cel -> convertCel pal cel trans base) cels

-- Convert cel to pnm, pnm to png, delete pnm
convertCel :: String -> String -> String -> String -> EitherT T.Text IO ()
convertCel palette cel bg base = do
  let celFile = base <> "/" <> cel <> ".cel"
  let pngFile = base <> "/" <> cel <> ".png"
  let paletteFile = base <> "/" <> palette
  runProgram ("analyzing cel " <> celFile) ("cel2pnm " <> paletteFile <> " " <> celFile <> " pnm")
  runProgram ("converting to png " <> pngFile) ("pnmtopng " <> " -transparent " <> bg <> " pnm > " <> pngFile)
  runProgram "removing temp file" "rm pnm"

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
