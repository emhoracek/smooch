{-# LANGUAGE OverloadedStrings #-}

module Shell where

import Kiss
import qualified Data.Text as Text
import System.Process
import System.Directory
import System.Exit
import Data.Monoid ((<>))
import Control.Monad.Trans.Either
import System.IO (hGetContents)

-- Gets the transparency color from a kcf palette.
transColor :: String -> IO (Either String String)
transColor paletteLoc = do
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
    ExitFailure n -> return $ Left ("Error while finding transparency color. Exit code: " <> show n <> ". Error: " <> errMsg)

-- Convert a whole list of cels given a palette. Put the files in target directory.
convertCels :: String -> [String] -> String -> IO (Either String ())
convertCels pal cels base = do
  eitherTrans <- runEitherT $ EitherT (transColor $ base ++ "/" ++ pal)
  let trans = case eitherTrans of 
                Right x -> x
                Left error -> "rgb:00/00/00"
  runEitherT $ mapM_ (\ cel -> EitherT $ convertCel pal cel trans base) cels

-- Convert cel to pnm, pnm to png, delete pnm
convertCel :: String -> String -> String -> String -> IO (Either String ())
convertCel palette cel bg base = runEitherT $ do
  let celFile = base <> "/" <> cel <> ".cel"
  let pngFile = base <> "/" <> cel <> ".png"
  let paletteFile = base <> "/" <> palette
  runProgram ("analyzing cel " <> celFile) ("cel2pnm " <> paletteFile <> " " <> celFile <> " pnm")
  runProgram ("converting to png " <> pngFile) ("/usr/bin/pnmtopng " <> " -transparent " <> bg <> " pnm > " <> pngFile)
  runProgram "removing temp file" "rm pnm"

runProgram :: String -> String -> EitherT String IO ()
runProgram name process = EitherT $ do
  (_, _, err, ph) <- createProcess (shell process) { std_err = CreatePipe }
  result <- waitForProcess ph
  errMsg <- case err of 
              Just x  -> hGetContents x
              Nothing -> return "no error message"
  case result of 
    ExitSuccess   -> return $ Right ()
    ExitFailure n -> return $ Left ("Error while " <> name <> ". Exit code: " <> show n <> ". Error: " <> errMsg)
