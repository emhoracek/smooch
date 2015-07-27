module CreateHTML (writeHTML) where

import Kiss
import Data.List (intercalate)

celHTML :: [KissCell] -> FilePath -> String
celHTML xs dir = 
    concatMap 
    (\x -> "<img src=\"" ++ dir ++ celName x ++ ".png\" id=\"" ++ celName x ++ "\"> \n") 
    (reverse xs)

writeHTML :: [KissCell] -> FilePath -> FilePath -> IO ()
writeHTML cels srcdir outdir = do
  header <- readFile $ srcdir ++ "/resources/header.html"
  footer <- readFile $ srcdir ++ "/resources/footer.html"
  writeFile outdir $ intercalate "\n"  [header, celHTML cels outdir, footer]

