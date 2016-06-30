{-# LANGUAGE OverloadedStrings #-}

module UploadSpec where

import           Control.Monad.Trans.Either
import           Kiss
import           Network.Wai.Parse
import           Test.Hspec
import           Upload

sampleCell :: KissCell
sampleCell = KissCell 0 "shirt"  0 [0,1,2,3] 0 (Position 0 0)

{--

shouldSucceed :: EitherT T.Text a -> a -> ??
shouldSucceed m r =
  if runEitherT m == Right ()
    then True
    else False

shouldSucceedWith :: EitherT T.Text a -> a -> Bool???
shouldSucceedWith m r =
  if runEitherT m == Right r
    then True
    else False

shouldFailWith :: EitherT T.Text a  -> T.Text -> Bool???
shouldFailWith m r =
  if runEitherT m == Left r
    then True
    else False

--}

spec = do
  describe "tryIO" $ do
    it "returns a Right () if the action completed" $
      runEitherT (tryIO $ return ()) `shouldReturn`
        Right ()
    it "returns an exception as Text if not" $
      runEitherT (tryIO $ readFile "potato" >> return ()) `shouldReturn`
        Left "potato: openFile: does not exist (No such file or directory)"


  {-- describe "getRelDir" $ do
    it "gets the path of a directory for the set, relative to static directory" $
      runEitherT (getRelDir [("file.lzh", FileInfo "file.lzh" "lzh" "content")]) `shouldReturn`
        Right "sets/file"
    it "returns an error message if the list of files is empty" $
      runEitherT (getRelDir []) `shouldReturn` Left "Please upload exactly one file."
  describe "getCNF" $ do
    it "returns the contents of the first CNF file it finds" $
      runEitherT (getCNF "tests/samples") `shouldReturn` Right "okay\n"
    it "gives an error if no CNF is found" $
      runEitherT (getCNF "./") `shouldReturn` Left "No configuration file found."
  describe "processSet" $ do
    it "does a tooooooon of shit, fuck" $
      pendingWith "fuuuuuuuuuuuuuuuck"
--}