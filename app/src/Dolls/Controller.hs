{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Dolls.Controller where

import           Control.Lens               ((^.))
import           Control.Monad.Trans.Except (runExceptT)
import           Crypto.Hash.MD5            (hashlazy)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Base16     as BS16
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import qualified Network.Wreq               as Wreq
import           Network.Wai                (Response)
import           Text.ParserCombinators.Parsec
import           Web.Fn
import           Web.Larceny                (subs, textFill)

import           Ctxt
import           Kiss
import           Dolls.Model
import           Dolls.View
import           Upload
import           Users.Model
import           Users.View

mkDoll :: String
       -> Maybe Text
       -> BS.ByteString
       -> Either Text FilePath
       -> NewDoll
mkDoll name otakuworldUrl hash eLoc = do
  case eLoc of
    Left err ->
      NewDoll (T.pack name) otakuworldUrl hash Nothing (Just err)
    Right loc ->
      NewDoll (T.pack name) otakuworldUrl hash (Just (T.pack loc)) Nothing

fileUploadHandler :: Ctxt -> File -> IO (Maybe Response)
fileUploadHandler ctxt (File name _ filePath') = do
  output <- runExceptT $ processDoll Nothing
                                     (T.unpack name, filePath')
  renderKissDoll ctxt output

linkUploadHandler :: Ctxt -> Text -> IO (Maybe Response)
linkUploadHandler ctxt link = do
  let mOtakuWorldUrl = otakuWorldUrl link
  case mOtakuWorldUrl of
    Right (mDir, dollname) -> do
      mExistingDoll <- getDollByOWUrl ctxt link
      case mExistingDoll of
        Nothing -> do
          let filename = dollname ++ ".lzh"
          let filepath = maybe filename (\d -> d ++ "/" ++ filename) mDir
          resp <- Wreq.get ("http://otakuworld.com/data/kiss/data/" ++ filepath)
          let body = resp ^. Wreq.responseBody
          let filehash = BS16.encode $ hashlazy body
          LBS.writeFile ("static/sets/" ++ filename) body
          output <- runExceptT $ processDoll Nothing
                                            (filename, "static/sets/" ++ filename)
          let newDoll = mkDoll dollname (Just link) filehash (fst <$> output)
          createDoll ctxt newDoll
          renderKissDoll ctxt output
        Just doll ->
          case (dollLocation doll, dollError doll) of
            (Just loc, Nothing) -> do
              output <- runExceptT $ getCels (T.unpack loc)
              renderKissDoll ctxt output
            (_, Just err) -> do
              renderKissDoll ctxt (Left err)
            _ -> renderKissDoll ctxt (Left "Something went wrong")

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
  output <- runExceptT $ processDoll (Just (userUsername user))
                                     (T.unpack name, filePath')
  renderKissDoll ctxt output

userDollHandler :: User -> Ctxt -> T.Text -> IO (Maybe Response)
userDollHandler user ctxt setName = do
  let userDir = staticUserDir (userUsername user)
  let staticDir = staticDollDir userDir (T.unpack setName)
  output <- (fmap . fmap) (staticDir,) (runExceptT $ createCels staticDir)
  -- The previous line is a bit weird.
  -- the result of runEitherT is an `IO (Either Text [KissCel])`.
  -- `renderKissDoll` wants an `Either Text (FilePath, [KissCel])`
  -- The `(staticDir,)` part uses Tuple Sections to turn the directory and a
  -- cel into a tuple.
  -- `(fmap . fmap)` let's us map into two layers of functions -- first the
  -- IO functor, then then Either functor. This makes the Right side of
  -- the Either a tuple! Whew.
  renderKissDoll ctxt output

renderKissDoll :: Ctxt -> Either T.Text (FilePath, [KissCel]) -> IO (Maybe Response)
renderKissDoll ctxt eOutputError =
  case eOutputError of
    Right (staticDir, cels) ->
      renderWith ctxt ["users", "kiss-set"] $ setSplices staticDir cels
    Left  e -> errText e
