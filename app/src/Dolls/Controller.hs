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
import           System.FilePath            (takeBaseName)
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
  fileContent <- LBS.readFile filePath'
  output <- createOrLoadDoll ctxt Nothing (takeBaseName (T.unpack name)) Nothing fileContent
  renderKissDoll ctxt output

linkUploadHandler :: Ctxt -> Text -> IO (Maybe Response)
linkUploadHandler ctxt link = do
  let mOtakuWorldUrl = otakuWorldUrl link
  case mOtakuWorldUrl of
    Right dollname -> do
      result <- do
        mExistingUrlDoll <- getDollByOWUrl ctxt link
        case mExistingUrlDoll of
          Nothing -> do
            resp <- Wreq.get (T.unpack link)
            let body = resp ^. Wreq.responseBody
            LBS.writeFile ("static/sets/" ++ dollname ++ ".lzh") body
            createOrLoadDoll ctxt Nothing dollname (Just link) body
          Just doll -> getDollFiles doll
      renderKissDoll ctxt result
    Left _ -> renderWith ctxt ["index"] errorSplices
  where
    errorSplices =
         subs [("linkErrors", textFill "Invalid OtakuWorld URL")]
           <> createUserErrorSplices
    otakuWorldUrl url = parse parseUrl "Invalid OtakuWorld url: " (T.unpack url)
    parseUrl = do
      string "http"
      optional (char 's')
      string "://otakuworld.com/data/kiss/data/"
      optional $ do
        alphaNum
        char '/'
      filename <- many (alphaNum <|> oneOf "_-")
      string ".lzh"
      return filename

-- Looks up the doll by the filehash. If doll with that hash already
-- exists, it reads the CNF and returns a directory where the cels
-- are stored plus a list of the cels. If no doll with that hash
-- already exists, then it creates a new doll and processes it.
createOrLoadDoll :: Ctxt
                 -> Maybe Text
                 -> [Char]
                 -> Maybe Text
                 -> LBS.ByteString
                 -> IO (Either Text (FilePath, [KissCel]))
createOrLoadDoll ctxt mUser dollname mLink body = do
  let hash = BS16.encode $ hashlazy body
  mExistingHashDoll <- getDollByHash ctxt hash
  case mExistingHashDoll of
    Nothing -> do
      let filename = dollname ++ ".lzh"
      output <- runExceptT $ processDoll mUser
                                         (filename, "static/sets/" ++ filename)
      let newDoll = mkDoll dollname mLink hash (fst <$> output)
      created <- createDoll ctxt newDoll
      if created then return output else return (Left "Something went wrong")
    Just doll -> do
      mUpdatedDoll <- maybe (return (Just doll))
                            (updateDollWithUrl ctxt doll dollname)
                            mLink
      case mUpdatedDoll of
        Just updatedDoll -> getDollFiles updatedDoll
        Nothing -> getDollFiles doll

getDollFiles :: Doll -> IO (Either Text (FilePath, [KissCel]))
getDollFiles doll =
  case (dollLocation doll, dollError doll) of
    (Just loc, Nothing) -> runExceptT $ getCels (T.unpack loc)
    (_, Just err) -> return (Left err)
    _ -> return (Left "Something went wrong")

userUploadHandler :: User -> Ctxt -> File -> IO (Maybe Response)
userUploadHandler user ctxt (File name _ filePath') = do
  fileContent <- LBS.readFile filePath'
  let mUsername = Just (userUsername user)
  let dollname = takeBaseName (T.unpack name)
  output <- createOrLoadDoll ctxt mUsername dollname Nothing fileContent
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
