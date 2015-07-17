module CreateHTML where

import ParseCNF
import Kiss

htmlCelImages :: [KissCell] -> String
htmlCelImages xs = 
    concatMap 
    (\x -> "<img src=\"" ++ celName x ++ ".png\" id=\"" ++ celName x ++ "\"> \n") 
    (reverse xs)

