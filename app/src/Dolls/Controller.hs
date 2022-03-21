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
import qualified Data.Text.Encoding         as T
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
       -> NewDoll
mkDoll name otakuworldUrl hash = do
  NewDoll (T.pack name) otakuworldUrl hash Nothing

hashFile :: LBS.ByteString -> BS.ByteString
hashFile = BS16.encode . hashlazy

fileUploadHandler :: Ctxt -> File -> IO (Maybe Response)
fileUploadHandler ctxt (File name _ filePath') = do
  hash <- hashFile <$> LBS.readFile filePath'
  output <- runExceptT $ createOrLoadDoll ctxt (takeBaseName (T.unpack name)) Nothing hash filePath'
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
            let lzhPath = "static/sets/" ++ dollname ++ ".lzh"
            liftIO $ LBS.writeFile lzhPath body
            let hash = hashFile body
            createOrLoadDoll ctxt dollname (Just link) hash lzhPath
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
                 -> [Char]
                 -> Maybe Text
                 -> BS.ByteString
                 -> FilePath
                 -> ExceptT Text IO DollData
createOrLoadDoll ctxt dollname mLink hash lzhPath = do
  mExistingHashDoll <- liftIO $ getDollByHash ctxt hash
  case mExistingHashDoll of
    Nothing ->
      let newDoll = mkDoll dollname mLink hash in
        processNewDoll ctxt newDoll lzhPath
    Just doll -> loadExistingDoll ctxt doll dollname mLink

processNewDoll :: Ctxt
               -> NewDoll
               -> FilePath
               -> ExceptT Text IO DollData
processNewDoll ctxt newDoll lzhPath = do
  let loc = T.unpack (T.decodeUtf8 (newDollHash newDoll))
  output <-  processDoll loc lzhPath (loc <> ".lzh")
  created <- liftIO $ createDoll ctxt newDoll
  if created then return output else throwE "Something went wrong"

loadExistingDoll :: Ctxt
                 -> Doll
                 -> String
                 -> Maybe Text
                 -> ExceptT Text IO DollData
loadExistingDoll ctxt existingDoll dollname mLink = do
  mUpdatedDoll <-
    case mLink of
      Nothing -> return (Just existingDoll)
      Just link -> liftIO (updateDollWithUrl ctxt existingDoll dollname link)
  maybe (getCels existingDoll) getCels mUpdatedDoll

-- TODO: Add join between users and dolls
userUploadHandler :: User -> Ctxt -> File -> IO (Maybe Response)
userUploadHandler _user ctxt (File name _ filePath') = do
  let dollname = takeBaseName (T.unpack name)
  hash <- hashFile <$> liftIO (LBS.readFile filePath')
  output <- runExceptT $ createOrLoadDoll ctxt dollname Nothing hash filePath'
  renderKissDoll ctxt output

-- TODO: This should use `getCels` instead of create cels, but
-- we can implement that when we add a relation between Users and
-- dolls in the database
userDollHandler :: User -> Ctxt -> T.Text -> IO (Maybe Response)
userDollHandler user ctxt setName = do
  let staticDir = T.unpack $ "static/sets/" <> setName
  output <- (fmap . fmap) (staticDir,) (runExceptT $ createCels staticDir)
  -- The previous line is a bit weird.
  -- the result of runEitherT is an `IO (Either Text [KissCel])`.
  -- `renderKissDoll` wants an `Either Text (FilePath, [KissCel])`
  -- The `(staticDir,)` part uses Tuple Sections to turn the directory and a
  -- cel into a tuple.
  -- `(fmap . fmap)` let's us map into two layers of functions -- first the
  -- IO functor, then then Either functor. This makes the Right side of
  -- the Either a tuple! Whew.
  renderKissDoll ctxt (toData <$> output)
  where
    toData :: (FilePath, [KissCel]) -> DollData
    toData (fp, cels) = DollData fp cels []

renderKissDoll :: Ctxt -> Either T.Text DollData -> IO (Maybe Response)
renderKissDoll ctxt eOutputError =
  case eOutputError of
    Right dollData ->
      renderWith ctxt ["users", "kiss-set"] $ setSplices dollData
    Left  e -> errText e
