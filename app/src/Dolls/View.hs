{-# LANGUAGE OverloadedStrings #-}

module Dolls.View where

import qualified Data.Text   as T
import           System.FilePath            (takeBaseName)
import           Web.Larceny

import           Ctxt
import           Kiss

setSplices :: DollData -> Substitutions Ctxt
setSplices (DollData staticDir cs sounds) =
  subs [("set-listing", setListingSplice),
        ("base", textFill (T.pack staticDir)),
        ("cel-images", celsSplice staticDir cs),
        ("sound-files", soundsSplice staticDir sounds)]

setListingSplice :: Fill Ctxt
setListingSplice =
  mapSubs toSet ([0..9] :: [Int])
  where toSet n =
          subs [("set-number", (textFill . T.pack . show) n)]

celsSplice :: FilePath -> [KissCel] -> Fill Ctxt
celsSplice dir cels =
  mapSubs (celImageSplice dir) (reverse cels)

celImageSplice :: FilePath -> KissCel -> Substitutions Ctxt
celImageSplice dir cel =
  subs [("cel-name", textFill $ T.pack $ celName cel)
       ,("pal-num", textFill $ T.pack $ show $ celPalette cel)
       ,("pal-dir", textFill $ T.pack dir <> "/palette" <> T.pack (show $ celPalette cel))]

soundsSplice :: FilePath -> [FilePath] -> Fill Ctxt
soundsSplice dir = mapSubs (soundSplice dir)

soundSplice :: FilePath -> FilePath -> Substitutions Ctxt
soundSplice dir sound =
  let soundId = takeBaseName sound in
  subs [("sound-file", textFill $ T.pack sound)
       ,("sound-id", textFill $ T.pack soundId)
       ,("sound-dir", textFill $ T.pack dir)]

linkUploadSplices :: Substitutions Ctxt
linkUploadSplices =
  subs [("linkErrors", textFill "")]