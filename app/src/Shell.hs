{-# LANGUAGE OverloadedStrings #-}

module Shell where

import           Control.Monad              (void)
import           Control.Monad.Trans.Either
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           System.Exit                (ExitCode (..))
import           System.IO                  (hGetContents)
import           System.Process             hiding (runCommand)

runCommand :: String -> String -> EitherT T.Text IO T.Text
runCommand name process = EitherT $ do
  (_, out, err, ph) <- createProcess (shell process) { std_out = CreatePipe, std_err = CreatePipe }
  result <- waitForProcess ph
  errMsg <- case err of
              Just x  -> hGetContents x
              Nothing -> return "no error message"
  outMsg <- case out of
              Just x -> hGetContents x
              Nothing -> return "no output"
  case result of
    ExitSuccess   -> return $ Right $ T.pack outMsg
    ExitFailure n -> return $ Left $ T.pack ("Error while " <> name <> ". Exit code: " <> show n <> ". Error: " <> errMsg)

unzipFile :: FilePath -> FilePath -> EitherT T.Text IO ()
unzipFile name dir = void $ runCommand ("decompressing archive " <> name)
                                       ("lha -xw=" <> dir <> " static/sets/" <> name)
