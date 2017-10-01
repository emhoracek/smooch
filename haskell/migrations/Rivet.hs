{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF rivet-autoimporter #-}
module Main where

import qualified Configuration.Dotenv
import           Control.Monad                     (when)
import qualified Data.Configurator                 as C
import           Data.Monoid
import           Database.Rivet.Adaptor.PostgreSQL
import qualified Database.Rivet.Main               as Rivet
import           System.Directory                  (doesFileExist)
import           System.Environment


main :: IO ()
main = do e <- doesFileExist ".env"
          when e $ Configuration.Dotenv.loadFile False ".env"
          args <- getArgs
          let (env, mode) =
               case args of
                 [env', "up"] -> (env', Rivet.MigrateUp)
                 [env', "down"] -> (env', Rivet.MigrateDown)
                 [env', "status"] -> (env', Rivet.MigrateStatus)
                 _ -> error "Usage: [executable] [devel|prod|...] [up|down|status]"
          conf <- C.load [C.Required (env <> ".cfg")]
          host <- C.require conf "postgresql-simple.host"
          port <- C.require conf "postgresql-simple.port"
          user <- C.require conf "postgresql-simple.user"
          pass <- C.require conf "postgresql-simple.pass"
          db <- C.require conf "postgresql-simple.db"
          adaptor <- setup id (ConnectInfo host port user pass db)
          Rivet.main adaptor mode migrations