{-# LANGUAGE OverloadedStrings #-}

module Javascript where

import ParseCNF
import Data.List 
import Text.ParserCombinators.Parsec
import Data.Aeson.Encode (encode)
import Data.ByteString.Lazy.Char8 as BS (writeFile)

{--
instance Show KissData where
    show x =
        show (kBorder x) ++ "\n" ++
        show (kWindowSize x) ++ "\n" ++
        concatMap show (kPalettes x) ++ "\n" ++
        intercalate "\n" (map show (kObjects x)) ++ "\n" ++
        intercalate "\n" (map show (kSets x)) --}


-- this sets how big the display is
jsDisplaySize :: KissData -> String
jsDisplaySize x = "this.displaySize = { x: " ++ 
                 show (fst $ kWindowSize x) ++
                 ", y: " ++ show (snd $ kWindowSize x) ++
                 " };"

jsSetImages :: KissData -> String
jsSetImages x = concatMap jsObjImages (kObjects x)

jsObjImages :: KissObject -> String
jsObjImages x = concatMap jsCellImage (objCells x)

jsCellImage :: KissCell -> String
jsCellImage x = "this." ++ celName x ++ " = new Image(); \n" ++
                "this." ++ celName x ++ ".src = \"" ++ 
                    celFile ++ "\";\n"
                where celFile = celName x ++ ".png"

writeJS f = do
  let kissData = getKissData f
  let json = encode kissData
  BS.writeFile "kiss.js" json

{--
main = do
    f <- readFile "color.cnf"
    let kissdata = getKissData f
    let json = encode kissdata
    BS.writeFile "kiss.js" json
    putStrLn "yay"
--}
