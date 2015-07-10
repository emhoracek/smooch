module CreateHTML where

import ParseCNF
import Kiss

{--
htmlSetImages :: KissData -> String
htmlSetImages x = concatMap htmlObjImages (kObjects x)

htmlObjImages :: KissObject -> String
htmlObjImages x = concatMap htmlCellImage (objCells x)
--} 

htmlCellImages :: [KissCell] -> String
htmlCellImages xs = 
    concatMap 
    (\x -> "<img src=\"" ++ celName x ++ ".png\" id=\"" ++ celName x ++ "\"> \n") 
    (reverse xs)

