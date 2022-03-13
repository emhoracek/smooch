{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Ctxt where

import           Control.Lens
import           Data.Pool
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vault.Lazy            as V
import qualified Database.PostgreSQL.Simple as PG
import           Network.Wai                (Response, pathInfo)
import           Network.Wai.Session        (Session)
import           Web.Fn
import           Web.Larceny                hiding (renderWith)
import qualified Web.Larceny                as L

type SmoochSession = Session IO Text (Maybe Text)

data Ctxt = Ctxt { _req           :: FnRequest
                 , _sessionKey    :: V.Key SmoochSession
                 , _library       :: Library Ctxt
                 , _substitutions :: Substitutions Ctxt
                 , _skipLogin     :: Maybe Text
                 , _pool          :: Pool PG.Connection }
makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

larcenyServe :: Ctxt -> IO (Maybe Response)
larcenyServe ctxt =
  let reqPath      = pathInfo . fst . getRequest $ ctxt in
    renderWith ctxt reqPath mempty

renderWith :: Ctxt -> Path -> Substitutions Ctxt -> IO (Maybe Response)
renderWith ctxt tplPath addSubs = do
  mRendered <- L.renderWith (ctxt ^. library)
                            (ctxt ^. substitutions <> addSubs)
                            ctxt
                            tplPath
  case mRendered of
    Nothing -> return Nothing
    Just rendered -> okHtml rendered

tshow :: Int -> Text
tshow = T.pack . show
