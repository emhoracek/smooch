{-# LANGUAGE OverloadedStrings #-}

module Common (fnTests) where

import           Control.Lens               ((^.))
import           Control.Monad              (void)
import           Data.Pool                  (withResource)
import qualified Database.PostgreSQL.Simple as PG
import           Test.Hspec
import           Test.Hspec.Fn

import           Ctxt
import           Web

fnTests :: SpecWith (FnHspecState Ctxt) -> Spec
fnTests fnSpecs = do
  ctxt <- runIO initializer
  fn (return ctxt) appBase [] (const $ return ()) (afterEval clearTables $ fnSpecs)

clearTables :: Ctxt -> IO ()
clearTables ctxt =
  withResource (ctxt ^. pool) $ \c ->
                        do void $ PG.execute_ c "delete from users"
