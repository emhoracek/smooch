{-# LANGUAGE OverloadedStrings #-}

module Shell where

import           Control.Monad              (void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           System.Exit                (ExitCode (..))
import           System.IO                  (hGetContents)
import           System.Process             hiding (runCommand)

import           Kiss
import           ParseCNF

-- Gets the transparency color from a kcf palette.
transColor :: PaletteFilename -> ExceptT T.Text IO String
transColor paletteLoc =
  let name = "finding transparency color"
      command = "cel2pnm -t " <> paletteLoc in
  T.unpack <$> runCommand name command


-- Gets an indexed color from a kcf palette.
colorByIndex :: Int -> PaletteFilename -> ExceptT T.Text IO String
colorByIndex colorNum paletteLoc =
  let name = "finding background color"
      command = "cel2pnm -c " <> show colorNum <> " " <> paletteLoc in
  T.unpack <$> runCommand name command

-- Convert a whole list of cels given a palette. Put the files in target directory. Return list of cels with offset information
convertCels :: Palettes
            -> [CNFKissCel]
            -> String
            -> ExceptT T.Text IO [ (String, (Int, Int)) ]
convertCels pals cels base = do
  mapM (\(CNFKissCel _ name pal _ _) -> convertCel pals pal name base) cels

-- Convert cel to pnm, pnm to png, delete pnm
convertCel :: Palettes
           -> Int
           -> CelFilename
           -> String
           -> ExceptT T.Text IO (String, (Int, Int))
convertCel palettes palNum cel base = do
  pal <- lookupPalette palNum palettes
  trans <- transColor $ base ++ "/" ++ pal
  let celFile = base <> "/" <> cel <> ".cel"
  let pngFile = base <> "/" <> cel <> ".png"
  let paletteFile = base <> "/" <> pal
  offsets <- convertAndGetOffsets paletteFile celFile
  void $ runCommand ("converting to png " <> pngFile)
                    ("pnmtopng " <> " -transparent " <> trans <> " pnm > " <> pngFile)
  void $ runCommand "removing temp file" "rm pnm"
  return (cel, offsets)

-- Gets the transparency color from a kcf palette.
convertAndGetOffsets :: PaletteFilename -> CelFilename -> ExceptT T.Text IO (Int, Int)
convertAndGetOffsets paletteFile celFile = do
  let name = "converting cel " <> celFile
      command = "cel2pnm -o " <> paletteFile <> " " <> celFile <> " pnm"
  offsetText  <- runCommand name command
  case T.words offsetText of
    (x: y: _ ) -> return (read $ T.unpack x, read $ T.unpack y)
    _          -> throwE "Bad offset output "

runCommand :: String -> String -> ExceptT T.Text IO T.Text
runCommand name process = do
  (_, out, err, ph) <- liftIO $ createProcess (shell process) { std_out = CreatePipe, std_err = CreatePipe }
  result <- liftIO $ waitForProcess ph
  errMsg <- case err of
              Just x  -> liftIO $ hGetContents x
              Nothing -> return "no error message"
  outMsg <- case out of
              Just x -> liftIO $ hGetContents x
              Nothing -> return "no output"
  case result of
    ExitSuccess   -> return $ T.pack outMsg
    ExitFailure n -> throwE $ T.pack ("Error while " <> name <> ". Exit code: " <> show n <> ". Error: " <> errMsg)

unzipFile :: FilePath -> FilePath -> ExceptT T.Text IO ()
unzipFile name dir = void $ runCommand ("decompressing archive " <> name)
                                       ("lha -xw=" <> dir <> " static/sets/" <> name)
