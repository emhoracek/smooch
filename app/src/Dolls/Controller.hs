{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Dolls.Controller where

import           Control.Lens               ((^.))
import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import qualified Network.Wreq               as Wreq
import           Network.Wai                (Response)
import           Text.ParserCombinators.Parsec
import           Web.Fn
import           Web.Larceny                (subs, textFill)

import           Ctxt
import           Kiss
import           Dolls.View
import           Upload
import           Users.Model
import           Users.View


fileUploadHandler :: Ctxt -> File -> IO (Maybe Response)
fileUploadHandler ctxt (File name _ filePath') = do
  output <- runExceptT $ processSet Nothing
                                    (T.unpack name, filePath')
  renderKissSet ctxt output

linkUploadHandler :: Ctxt -> Text -> IO (Maybe Response)
linkUploadHandler ctxt link = do
  let mOtakuWorldUrl = otakuWorldUrl link
  case mOtakuWorldUrl of
    Right (mDir, dollname) -> do
      let filename = dollname ++ ".lzh"
      let filepath = maybe filename (\d -> d ++ "/" ++ filename) mDir
      resp <- Wreq.get ("http://otakuworld.com/data/kiss/data/" ++ filepath)
      BS.writeFile ("static/sets/" ++ filename)  (resp ^. Wreq.responseBody)
      output <- runExceptT $ processSet Nothing
                                        (dollname, "static/sets/" ++ filename)
      renderKissSet ctxt output
    Left _ -> renderWith ctxt ["index"] errorSplices
  where
    errorSplices =
         subs [("linkErrors", textFill "Invalid OtakuWorld URL")]
           <> createUserErrorSplices

otakuWorldUrl :: Text -> Either ParseError (Maybe String, String)
otakuWorldUrl url = parse parseUrl "Invalid OtakuWorld url: " (T.unpack url)
  where parseUrl :: Parser (Maybe String, String)
        parseUrl = do
          string "http"
          optional (char 's')
          string "://otakuworld.com/data/kiss/data/"
          mDir <- option Nothing $ do
            dir <- alphaNum
            char '/'
            return $ Just [dir]
          filename <- many alphaNum
          string ".lzh"
          return (mDir, filename)

userUploadHandler :: User -> Ctxt -> File -> IO (Maybe Response)
userUploadHandler user ctxt (File name _ filePath') = do
  output <- runExceptT $ processSet (Just (userUsername user))
                                    (T.unpack name, filePath')
  renderKissSet ctxt output

userSetHandler :: User -> Ctxt -> T.Text -> IO (Maybe Response)
userSetHandler user ctxt setName = do
  let userDir = staticUserDir (userUsername user)
  let staticDir = staticSetDir userDir (T.unpack setName)
  output <- (fmap . fmap) (staticDir,) (runExceptT $ createCels staticDir)
  -- The previous line is a bit weird.
  -- the result of runEitherT is an `IO (Either Text [KissCel])`.
  -- `renderKissSet` wants an `Either Text (FilePath, [KissCel])`
  -- The `(staticDir,)` part uses Tuple Sections to turn the directory and a
  -- cel into a tuple.
  -- `(fmap . fmap)` let's us map into two layers of functions -- first the
  -- IO functor, then then Either functor. This makes the Right side of
  -- the Either a tuple! Whew.
  renderKissSet ctxt output

renderKissSet :: Ctxt -> Either T.Text (FilePath, [KissCel]) -> IO (Maybe Response)
renderKissSet ctxt eOutputError =
  case eOutputError of
    Right (staticDir, cels) ->
      renderWith ctxt ["users", "kiss-set"] $ setSplices staticDir cels
    Left  e -> errText e
