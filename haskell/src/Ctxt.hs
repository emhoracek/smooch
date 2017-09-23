{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Ctxt where

import           Control.Lens
import           Data.Pool
import qualified Database.PostgreSQL.Simple as PG
import           Web.Fn
import           Web.Fn.Extra.Heist

data Ctxt = Ctxt { _req   :: FnRequest,
                   _heist :: FnHeistState Ctxt,
                   _pool  :: Pool PG.Connection }
makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

instance HeistContext Ctxt where
  getHeist = _heist
