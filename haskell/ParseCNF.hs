{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ParseCNF where

import Kiss
import Data.Text (pack)
import Text.ParserCombinators.Parsec
import Data.List
import Data.Maybe
import Data.Aeson

sampleCell1 :: String
sampleCell1 = "#19       markr6.cel      *0 : 0 1 2 3 4 5 6 7 8 9 &"
sampleCell2 :: String
sampleCell2 = "#25       tights1.cel     *0 : 0 1 2 3 4 5 6 7 8 9     ;%t75 &"
sampleCell3 :: String
sampleCell3 = "#180.99   handl.cel       *0 :   1     4     7 &"
sampleCells :: String
sampleCells = 
    "#1         markr6.cel          *0 : 0 1 2 3 4 5 6 7 8 9 \n" ++
    "#24        shirt1.cel          *0 : 0 1 2 3 4 5 6 7 8 9 ; top of shirt\n" ++
    "; these shirts are the same object \n" ++
    "#24        shirt1b.cel         *0 : 0 1 2 3 4 5 6 7 8 9 \n" ++
    "#180.99    handl.cel           *0 :   1     4     7 "

sampleSet1 :: String 
sampleSet1 = "$2 192,11 * 56,176 55,21 259,62 15,24 375,63"
sampleSet2 :: String
sampleSet2 = "$3 43,115 154,62 372,108 253,156 * * * 165,207" ++
             " * 162,198 * 119,56 152,44 * * * \n" ++
             " 16,355 394,362 108,355 * * * 125,261\n" ++
             "; blah blah commentcakes"
sampleSet3 :: String
sampleSet3 = "$0 192,11 * 56,176 55,21 259,62 15,24 375,63"

sampleKiss :: String
sampleKiss = 
  "; ** Palette files ** \n" ++ 
  "%colors.kcf \n" ++
  "(756,398) \n" ++
  "[ 0 \n" ++
  "#1   shirt.cel  *0 : 0 1 2 3 \n" ++
  "#2   body.cel   *0 : 0 1 2 3 \n" ++
  "#3   shirtb.cel *0 : 0 1 2 3 \n" ++
  "$0 * * *"

data CNFLine = CNFMemory Int
             | CNFBorder Int
             | CNFPalette String
             | CNFWindowSize (Int, Int)
             | CNFObject (Int, KissCell)
             | CNFSet KissSet
             | CNFComment
    deriving (Eq, Show)

getKissData :: String -> KissData
getKissData file = 
    let p = parse parseCNFLines "error" file in
    either errorKiss linesToScript p

errorKiss :: ParseError -> KissData
errorKiss s = BadKiss (show s)

errorKiss' :: ParseError -> [KissObject]
errorKiss' s = [BadObject (show s)]

errorKiss'' :: ParseError -> [KissCell]
errorKiss'' s = [BadCell (show s)]

-- Cell-based KiSS data
getKissCells :: String -> [KissCell]
getKissCells file = 
    let p = parse parseCNFLines "error" file in
    either errorKiss'' linesToCells p

-- Object-based KiSS data

linesToObjects :: [(Int, KissCell)] -> [KissSet] -> [KissObject]
linesToObjects xs ys = 
    map (\((x,y),z) -> KissObject x y z) objs
    where cells = nub $ map (\x -> combineCells (fst x) xs) xs
          positions = transpose (justPos ys)
          objs = if length cells <= length positions 
                  then zip cells positions
                  else zip cells (pad positions (length cells))
           
pad :: [[SetPos]] -> Int -> [[SetPos]]
pad xs 0 = xs
pad xs n = [NoPosition] : pad xs (n-1) 

combineCells :: Int -> [(Int, KissCell)] -> (Int, [KissCell])
combineCells n xs = (n, map snd (filter (\x -> fst x == n) xs))

justPos :: [KissSet] -> [[SetPos]]
justPos ys = map (\(KissSet pal pos) -> pos) ys

linesToScript :: [CNFLine] -> KissData
linesToScript xs = 
    KissData memory border palettes windowSize objects 
    where memory   = fromMaybe 0 (listToMaybe [ a | CNFMemory a <- xs ])
          border   = fromMaybe 0 (listToMaybe [ a | CNFBorder a <- xs ])
          palettes = [ a | CNFPalette a <- xs ]
          windowSize = fromMaybe (600, 480) (listToMaybe [ a | CNFWindowSize a <- xs ])
          objects = linesToObjects [ a | CNFObject a <- xs ] sets
          sets = [ a | CNFSet a <- xs ]
         
linesToScript' :: [CNFLine] -> [KissObject]
linesToScript' xs = linesToObjects [ a | CNFObject a <- xs ] [ a | CNFSet a <- xs ]

linesToCells :: [CNFLine] -> [KissCell]
linesToCells xs = [ snd a | CNFObject a <- xs ]
 
parseCellLine :: Parser CNFLine
parseCellLine = do
    char '#'
    num <- many digit
    cell <- parseCell
    optional parseComment
    return $ CNFObject (read num, cell)

parseCell :: Parser KissCell
parseCell = do
    fix <- option 0 parseFix
    skipMany1 space
    file <- many (choice [letter, digit, char '_', char '-'])
    string ".cel"
    skipMany1 space
    char '*'
    palette <- many1 digit
    skipMany space
    char ':'
    skipMany space
    sets <- option [] parseSets
    skipMany space
    transp <- option 0 (try parseTransp)
    return $ KissCell fix file (read palette) sets transp

parseFix :: Parser Int
parseFix = do
    char '.'
    num <- many digit
    return $ read num

parseSets :: Parser [Int]
parseSets = many1 parseSet

parseSet :: Parser Int
parseSet = do
    num <- digit
    skipMany space
    return $ read [num]

parseComment :: Parser String
parseComment = do    
    char ';'
    comment <- many (noneOf "\r\n")
    return comment

parseTransp :: Parser Int
parseTransp = do
    string ";%t"
    num <- many digit
    return $ read num

parseMemory :: Parser CNFLine
parseMemory = do
    char '='
    num <- many digit
    return $ CNFMemory (read num)

parseWindowSize :: Parser CNFLine
parseWindowSize = do
    char '('
    width <- many digit
    char ','
    height <- many digit
    char ')'
    return $ CNFWindowSize (read width, read height)

parsePalette :: Parser CNFLine
parsePalette = do
    char '%'
    filename <- many (choice [letter, digit, char '_', char '-', char '.'])
    return $ CNFPalette filename

parseBorder :: Parser CNFLine
parseBorder = do
    char '['
    num <- many digit
    return $ CNFBorder (read num)

parseKissSet :: Parser CNFLine
parseKissSet = do
    char '$'
    paletteGroup <- digit
    positions <- many parsePosorNoPos
    return $ CNFSet (KissSet (read [paletteGroup]) positions)

parsePosorNoPos :: Parser SetPos
parsePosorNoPos = do
    spaces
    position <- (parsePosition <|> parseNoPosition <?> "position")
    spaces
    return position

parsePosition :: Parser SetPos
parsePosition = do
    xpos <- many1 digit
    char ','
    ypos <- many1 digit
    return $ Position (read xpos) (read ypos)

parseNoPosition :: Parser SetPos
parseNoPosition = do
    char '*'
    return NoPosition

parseCNFLines :: Parser [CNFLine]
parseCNFLines = do
    lines <- many parseCNFLine
    eof
    return lines

parseCNFLine :: Parser CNFLine
parseCNFLine = do
    line <- choice [parseCellLine, 
                    parseKissSet,
                    parseMemory, parsePalette, 
                    parseBorder, parseWindowSize, 
                    parseCNFComment]
    spaces
    return line

parseCNFComment :: Parser CNFLine
parseCNFComment = do
    char ';'
    comment <- many (noneOf "\r\n")
    return CNFComment

parsePos :: Parser SetPos
parsePos = do
    skipMany space
    position <- parsePosition <|> parseNoPosition
    skipMany space
    return position

{--
main = do
    putStrLn "Parsing object lines:"
    print $ parse parseCellLine "error" sampleCell1
    print $ parse parseCellLine "error" sampleCell2
    print $ parse parseCellLine "error" sampleCell3
    putStrLn "Parsing set lines:"
    print $ parse parseKissSet "error" sampleSet1
    print $ parse parseKissSet "error" sampleSet2
    print $ parse parseKissSet "error" sampleSet3
    putStrLn "Parsing a real set: "
    f <- readFile "aurora/aurora.cnf"
    let p = parse parseCNFLines "error" f
    let l = either errorKiss' linesToScript' p
    let kissdata = either errorKiss linesToScript p
    print p
    putStrLn "objs"
    print l 
    putStrLn "all"
    print kissdata 
--}
