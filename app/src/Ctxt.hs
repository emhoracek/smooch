{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Ctxt where

import           Control.Lens
import           Data.Monoid                ((<>))
import           Data.Pool
import qualified Database.PostgreSQL.Simple as PG
import           Network.Wai                (Response)
import           Web.Fn
import           Web.Larceny                hiding (renderWith)
import qualified Web.Larceny                as L

data Ctxt = Ctxt { _req           :: FnRequest,
                   _library       :: Library Ctxt,
                   _substitutions :: Substitutions Ctxt,
                   _pool          :: Pool PG.Connection }
makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

renderWith :: Ctxt -> Path -> Substitutions Ctxt -> IO (Maybe Response)
renderWith ctxt tplPath addSubs = do
  mRendered <- L.renderWith (ctxt ^. library)
                            (ctxt ^. substitutions <> addSubs)
                            ctxt
                            tplPath
  case mRendered of
    Nothing -> return Nothing
    Just rendered -> okHtml rendered
