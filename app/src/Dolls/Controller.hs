{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Dolls.Controller where

import           Control.Lens               ((^.))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
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
       -> FilePath
       -> NewDoll
mkDoll name otakuworldUrl hash loc = do
  NewDoll (T.pack name) otakuworldUrl hash (Just (T.pack loc)) Nothing

hashFile :: LBS.ByteString -> BS.ByteString
hashFile = BS16.encode . hashlazy

fileUploadHandler :: Ctxt -> File -> IO (Maybe Response)
fileUploadHandler ctxt (File name _ filePath') = do
  hash <- hashFile <$> LBS.readFile filePath'
  output <- runExceptT $ createOrLoadDoll ctxt Nothing (takeBaseName (T.unpack name)) Nothing hash
  renderKissDoll ctxt output

linkUploadHandler :: Ctxt -> Text -> IO (Maybe Response)
linkUploadHandler ctxt link = do
  let mOtakuWorldUrl = otakuWorldUrl link
  case mOtakuWorldUrl of
    Right dollname -> do
      result <- runExceptT $ do
        mExistingUrlDoll <- liftIO $ getDollByOWUrl ctxt link
        case mExistingUrlDoll of
          Nothing -> do
            resp <- liftIO $ Wreq.get (T.unpack link)
            let body = resp ^. Wreq.responseBody
            liftIO $ LBS.writeFile ("static/sets/" ++ dollname ++ ".lzh") body
            let hash = hashFile body
            createOrLoadDoll ctxt Nothing dollname (Just link) hash
          Just doll -> getCels doll
      renderKissDoll ctxt result
    Left _ -> renderWith ctxt ["index"] errorSplices
  where
    errorSplices =
         subs [("linkErrors", textFill "Invalid OtakuWorld URL")]
           <> createUserErrorSplices
    otakuWorldUrl url = parse parseUrl "" (T.unpack url)
    parseUrl = do
      string "http://otakuworld.com/data/kiss/data/"
      optional $ alphaNum >> char '/'
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
                 -> BS.ByteString
                 -> ExceptT Text IO (FilePath, [KissCel])
createOrLoadDoll ctxt mUser dollname mLink hash = do
  mExistingHashDoll <- liftIO $ getDollByHash ctxt hash
  case mExistingHashDoll of
    Nothing ->
      let newDoll = mkDoll dollname mLink hash  ("static/sets/" ++ dollname) in
        processNewDoll ctxt mUser newDoll
    Just doll -> loadExistingDoll ctxt doll dollname mLink

processNewDoll :: Ctxt
               -> Maybe Text
               -> NewDoll
               -> ExceptT Text IO (FilePath, [KissCel])
processNewDoll ctxt mUser newDoll = do
  let filename = T.unpack (newDollName newDoll <> ".lzh")
  output <-  processDoll mUser
                (filename, "static/sets/" ++ filename)
  created <- liftIO $ createDoll ctxt newDoll
  if created then return output else throwE "Something went wrong"

loadExistingDoll :: Ctxt
                 -> Doll
                 -> String
                 -> Maybe Text
                 -> ExceptT Text IO (FilePath, [KissCel])
loadExistingDoll ctxt existingDoll dollname mLink = do
  mUpdatedDoll <-
    case mLink of
      Nothing -> return (Just existingDoll)
      Just link -> liftIO (updateDollWithUrl ctxt existingDoll dollname link)
  maybe (getCels existingDoll) getCels mUpdatedDoll

userUploadHandler :: User -> Ctxt -> File -> IO (Maybe Response)
userUploadHandler user ctxt (File name _ filePath') = do
  let mUsername = Just (userUsername user)
  let dollname = takeBaseName (T.unpack name)
  hash <- hashFile <$> liftIO (LBS.readFile filePath')
  output <- runExceptT $ createOrLoadDoll ctxt mUsername dollname Nothing hash
  renderKissDoll ctxt output

-- TODO: This should use `getCels` instead of create cels, but
-- we can implement that when we add a relation between Users and
-- dolls in the database
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
