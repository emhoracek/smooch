{-# LANGUAGE OverloadedStrings #-}

module KissSet where

import Prelude hiding (id, head, div)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import System.FilePath ((</>))
import Data.Monoid ((<>))
import Kiss

images base cels = mapM_ (\x -> img ! src (toSrc x) ! id (toId x)) cels
  where toSrc c = stringValue $ base </> celName c <> ".png"
        toId  c = stringValue $ celName c
        
render base cels = 
  html $ do
    head $ do
      meta ! charset "utf-8"
      H.title "Smooch"
      link ! rel "stylesheet" ! type_ "text/css" ! href "/screen.css"
      script ! type_ "text/javascript" $
        "window.onload = function() { document.getElementById('loading').style.display = 'none';};"
    body $ do
      div ! id "loading" $ progress ""
      div ! id "sets" $ ul $ 
        mapM_ (\x -> li $ a ! class_ "set" $ toHtml (T.pack $ show x)) [0..9]
      div ! id "borderarea" $ 
        div ! id "playarea" $  
          images base cels
      div ! id "nav" $ 
        button ! id "editbutton" $ "Edit"
      div ! id "toolbar" $ do 
        h1 "Edit" 
        div ! class_ "infobox" $ ""
      div ! id "footer" $ "From OtakuWorld"
      script ! type_ "text/javascript" ! src (stringValue $ base </> "setdata.js") $ ""
      script ! type_ "text/javascript" ! src "/kiss.js" $ ""
