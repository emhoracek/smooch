{-# LANGUAGE OverloadedStrings #-}

module Common (fnTests) where

import           Control.Lens               ((^.))
import           Control.Monad              (void)
import           Data.Pool                  (withResource)
import qualified Database.PostgreSQL.Simple as PG
import           System.Environment         (setEnv)
import           Test.Hspec
import           Test.Hspec.Fn

import           Ctxt
import           Web

fnTests :: SpecWith (FnHspecState Ctxt) -> Spec
fnTests fnSpecs = do
  runIO $ setEnv "ENV" "test"
  ctxt <- runIO initializer
  fn (return ctxt) -- how to set up the context
     appBase -- turn the context into a WAI application
     [] -- list of middleware
     (const $ return ()) -- clean up what to do after tests are over
     (afterEval clearTables $ fnSpecs) -- run the tests!

clearTables :: Ctxt -> IO ()
clearTables ctxt =
  withResource (ctxt ^. pool) $ \c ->
                        do void $ PG.execute_ c "delete from users"
