{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Dolls.ModelSpec where

import           Control.Monad          (void)
import           Crypto.Hash.MD5        (hashlazy)
import qualified Data.ByteString.Base16 as BS16
import           Test.Hspec
import Test.Hspec.Fn
    ( shouldEqual,
      eval,
      Factory(create, save, fields),
      shouldNotEqual )

import           Common
import           Ctxt
import           Dolls.Model

instance Factory Ctxt Doll NewDoll where
  fields = let hash =  BS16.encode $ hashlazy "foobar"
               url = "http://otakuworld.com/data/kiss/data/f/foobar.lzh" in
            NewDoll "foobar" (Just url) hash Nothing
  save newDoll = do Just doll <-
                        eval (\ctxt -> do
                                void $ createDoll ctxt newDoll
                                getDollByHash ctxt (newDollHash newDoll))
                    return doll

spec :: Spec
spec = fnTests $ do
  describe "getDolls" $ do
    it "should get all the dolls" $ do
      newDoll <- create id
      dolls <- eval getDolls
      (newDoll `elem` dolls) `shouldEqual` True
  describe "createDoll" $ do
    it "should create a new doll" $ do
      void $ eval (\ctxt -> createDoll ctxt
                              (NewDoll "new" Nothing "hash" Nothing))
      [doll] <- eval getDolls
      dollName doll `shouldEqual` "new"
  describe "getDollByHash" $ do
    it "should get the doll with that hash" $ do
      newDoll <- create (\doll -> doll { newDollHash = "some_hash" })
      Just doll <- eval (`getDollByHash` "some_hash")
      doll `shouldEqual` newDoll
  describe "getDollByOWUrl" $ do
    it "should get the doll with that url" $ do
      newDoll <- create (\doll -> doll { newDollOtakuWorldUrl = Just "http://blah" })
      Just doll <- eval (`getDollByOWUrl` "http://blah")
      doll `shouldEqual` newDoll
  describe "updateDollWithUrl" $ do
    it "should not change a doll with the same url" $ do
      newDoll <- create (\doll -> doll { newDollOtakuWorldUrl = Just "http://blah"
                                       , newDollName = "blah" })
      doll <- eval (\ctxt -> updateDollWithUrl
                             ctxt
                             newDoll
                             "blah"
                             "http://blah")
      doll `shouldEqual` Just newDoll
    it "should update the URL of a doll without a url" $ do
      newDoll <- create (\doll -> doll { newDollOtakuWorldUrl = Nothing
                                       , newDollName = "blah" })
      doll <- eval (\ctxt -> updateDollWithUrl
                             ctxt
                             newDoll
                             "blah"
                             "http://blah")
      doll `shouldNotEqual` Just newDoll
      (dollOtakuWorldUrl <$> doll) `shouldEqual` Just (Just "http://blah")
    it "should update the name of a doll with the wrong name" $ do
      newDoll <- create (\doll -> doll { newDollOtakuWorldUrl = Nothing
                                       , newDollName = "wrong" })
      doll <- eval (\ctxt -> updateDollWithUrl
                             ctxt
                             newDoll
                             "right"
                             "http://blah")
      doll `shouldNotEqual` Just newDoll
      (dollName <$> doll) `shouldEqual` Just "right"
