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
