{-# LANGUAGE OverloadedStrings #-}

module Shell where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Data.Char                  (toLower)
import qualified Data.Text                  as T
import           System.Directory           (listDirectory, renameFile)
import           System.Process


unzipFile :: String -> String -> ExceptT T.Text IO ()
unzipFile name dir = liftIO $ callProcess "lha" ["-xw=" <> dir, "static/sets/" <> name]

lowercaseFiles :: String ->IO ()
lowercaseFiles dir = do
  files <- listDirectory dir
  mapM_ (\f -> renameFile (dir <> "/" <> f) (dir <> "/" <> map toLower f)) files