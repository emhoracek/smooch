{-# LANGUAGE OverloadedStrings #-}

module Index where

import Prelude hiding (id, head, div)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

render = do
  html $ do
    head $ do
      H.title "Smooch"
    body $ do
      h1 "Smooch"
      h2 "Dress-up dolls!"
      H.form ! action "/upload" ! method "post" ! enctype "multipart/form-data" $ do
        input ! type_ "file" ! name "kissfile"
        input ! type_ "submit" ! value "Upload!" 
