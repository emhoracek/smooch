{-# LANGUAGE OverloadedStrings #-}

module Shell where

import           Control.Monad              (void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           System.Exit                (ExitCode (..))
import           System.IO                  (hGetContents)
import           System.Process

unzipFile :: String -> String -> ExceptT T.Text IO T.Text
unzipFile name dir = do
  (_, out, err, ph) <- liftIO $ createProcess (shell ("lha -xw=" <> dir <> " static/sets/" <> name)) { std_out = CreatePipe, std_err = CreatePipe }
  result <- liftIO $ waitForProcess ph
  errMsg <- case err of
              Just x  -> liftIO $ hGetContents x
              Nothing -> return "no error message"
  case result of
    ExitSuccess   -> return $ T.pack ""
    ExitFailure n -> throwE $ T.pack ("Error while decompressing archive " <> name <> ". Exit code: " <> show n <> ". Error: " <> errMsg)