{-# LANGUAGE OverloadedStrings #-}

module Sets.Controller where

import           Control.Monad.Trans.Either (runEitherT)
import qualified Data.Text                  as T
import           Network.Wai                (Response)
import           System.FilePath            (takeBaseName)
import           Web.Fn

import           Ctxt
import           Kiss
import           Sets.View
import           Upload
import           Users.Model

userUploadHandler :: User -> Ctxt -> File -> IO (Maybe Response)
userUploadHandler user ctxt (File name _ filePath') = do
  output <- runEitherT $ processUserSet (userUsername user)
                                        (T.unpack name, filePath')
  renderKissSet' ctxt output


userSetHandler :: User -> Ctxt -> T.Text -> IO (Maybe Response)
userSetHandler user ctxt setName = do
  let userDir = staticUserDir (userUsername user)
  let staticDir = staticUserSetDir userDir (T.unpack setName)
  output <- (fmap . fmap) ((,) staticDir) (runEitherT $ createCels staticDir)
  -- The previous line is a bit weird.
  -- the result of runEitherT is an `IO (Either Text [KissCell])`.
  -- `renderKissSet` wants an `Either Text (FilePath, [KissCell])`
  -- The `(,)` lets us turn two things into a tuple.
  -- `(fmap . fmap)` let's us map into two layers of functions -- first the
  -- IO functor, then then Either functor. This makes the Right side of
  -- the Either a tuple! Whew.
  renderKissSet' ctxt output

renderKissSet' :: Ctxt -> Either T.Text (FilePath, [KissCell]) -> IO (Maybe Response)
renderKissSet' ctxt eOutputError =
  case eOutputError of
    Right (staticDir, cels) ->
      renderWith ctxt ["users", "kiss-set"] $ setSplices staticDir cels
    Left  e -> errText e
