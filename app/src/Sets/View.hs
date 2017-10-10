{-# LANGUAGE OverloadedStrings #-}

module Sets.View where

import qualified Data.Text   as T
import           Web.Larceny

import           Ctxt
import           Kiss

setSplices :: String -> [KissCell] -> Substitutions Ctxt
setSplices staticDir cs =
  subs [("set-listing", setListingSplice),
        ("base", textFill (T.pack staticDir)),
        ("celImages", celsSplice staticDir cs)]

setListingSplice :: Fill Ctxt
setListingSplice =
  mapSubs toSet ([0..9] :: [Int])
  where toSet n =
          subs [("set-number", (textFill . T.pack . show) n)]

celsSplice :: FilePath -> [KissCell] -> Fill Ctxt
celsSplice dir cels =
  mapSubs (celImageSplice dir) (reverse cels)

celImageSplice :: FilePath -> KissCell -> Substitutions Ctxt
celImageSplice dir cel =
  subs [("cel-name", textFill $ T.pack $ celName cel)
       ,("dir", textFill $ T.pack dir)]
