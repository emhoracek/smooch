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

getKissData :: String -> KissData
getKissData file = 
    let p = parse parseCNFLines "error" file in
    either errorKiss linesToScript p

getKissCells :: String -> [KissCell]
getKissCells file = 
    let p = parse parseCNFLines "error" file in
    either errorCells linesToCells p

errorKiss :: ParseError -> KissData
errorKiss s = BadKiss (show s)

errorObjs :: ParseError -> [KissObject]
errorObjs s = [BadObject (show s)]

errorCells :: ParseError -> [KissCell]
errorCells s = [BadCell (show s)]

-- Converting CNF lines to useable KiSS data

linesToScript :: [CNFLine] -> KissData
linesToScript xs = 
    KissData memory border palettes windowSize objects 
    where -- memory and border are optional 
          memory   = fromMaybe 0 (listToMaybe [ a | CNFMemory a <- xs ])
          border   = fromMaybe 0 (listToMaybe [ a | CNFBorder a <- xs ])
          -- at least one palette must be specified
          palettes = [ a | CNFPalette a <- xs ]
          -- window is (600, 480) by default
          windowSize = fromMaybe (600, 480) (listToMaybe [ a | CNFWindowSize a <- xs ])
          -- turn object #, cell, set positions to objects 
          positions = [ a | CNFSetPos a <- xs ]
          objects = linesToObjects [ a | CNFCell a <- xs ] positions

linesToObjects :: [(Int, KissCell)] -> [KissSetPos] -> [KissObject]
linesToObjects xs ys = 
    map (\((x,y),z) -> KissObject x y z) (zipIt cells positions)
    where cells = nub $ map (\x -> combineCells (fst x) xs) xs
          positions = zip (sort $ listObjectIds xs) (cellPositions ys)
          -- nub removes duplicates from a list

listObjectIds :: [(Int,KissCell)] -> [Int]
listObjectIds xs = [ a | (a,b) <- xs ]

zipIt :: [(Int, [KissCell])] -> [(Int,[SetPos])] -> [((Int, [KissCell]), [SetPos])]
zipIt cells positions = [ (cell, snd pos) | cell <- cells, pos <- positions, fst cell == fst pos]

cellPositions :: [KissSetPos] -> [[SetPos]]
cellPositions xs = transpose $ map (\(KissSetPos pal pos) -> pos) xs

{--
zipIt cells ys 
  | length cells == length (cellPositions ys) = zip cells (cellPositions ys)
  | length cells > length (cellPositions ys)  = error errorMsg
  | otherwise                                 = error errorMsg
  where errorMsg = "Error: " ++ show (length cells) ++ " cells and " 
                      ++ show (length $ cellPositions ys) ++ " positions."
--}
--
combineCells :: Int -> [(Int, KissCell)] -> (Int, [KissCell])
combineCells n xs = (n, map snd (filter (\x -> fst x == n) xs))


-- A CNF's [KissSetPos] is 10 lists of SetPos each `#cells` long.
-- This turns them into `#cells` lists of 10 SetPos.
{--
cellPositions :: [KissSetPos] -> [[SetPos]]
cellPositions ys = transpose [head $ map (\(KissSetPos pal pos) -> reverse pos) ys]
--}
linesToCells :: [CNFLine] -> [KissCell]
linesToCells xs = [ snd a | CNFCell a <- xs ]
 
-- KiSS Parser Combinators

-- Parses the lines describing cells and objects.
parseCellLine :: Parser CNFLine
parseCellLine = do
    char '#'
    num <- many digit
    cell <- parseCell
    optional parseComment
    return $ CNFCell (read num, cell)

parseCell :: Parser KissCell
parseCell = do
    fix <- option 0 parseFix
    skipMany1 space
    file <- many1 (noneOf ". ")
    string ".cel" <?> "cel file extenstion"
    skipMany1 space
    palette <- option 0 (try parseCellPalette)
    skipMany space
    optional (char ':')
    skipMany space
    sets <- option [] parseSets
    skipMany space
    transp <- option 0 (try parseTransp)
    return $ KissCell fix file palette sets transp

parseCellPalette :: Parser Int
parseCellPalette = do
    char '*'
    num <- many1 digit
    return $ read num

parseFix :: Parser Int
parseFix = do
    char '.'
    num <- many digit
    return $ read num

-- This parses all the list of sets the cell will be displayed in
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
    many (noneOf "\r\n")

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

-- The following four functions parse the cel positions for each set of cells.
parseSetPos :: Parser CNFLine
parseSetPos = do
    char '$'
    paletteGroup <- digit
    positions <- many parsePosition
    return $ CNFSetPos (KissSetPos (read [paletteGroup]) positions)

parsePosition :: Parser SetPos
parsePosition = do
    spaces
    position <- parsePos <|> parseNoPos <?> "position"
    spaces
    return position

parsePos :: Parser SetPos
parsePos = do
    xpos <- many1 digit
    char ','
    ypos <- many1 digit
    return $ Position (read xpos) (read ypos)

parseNoPos :: Parser SetPos
parseNoPos = do
    char '*'
    return NoPosition

data CNFLine = CNFMemory Int
             | CNFBorder Int
             | CNFPalette String
             | CNFWindowSize (Int, Int)
             | CNFCell (Int, KissCell)
             | CNFSetPos KissSetPos
             | CNFComment
    deriving (Eq, Show)

-- Parse the CNF one "line" (including mult-line set descriptions) at a time
parseCNFLines :: Parser [CNFLine]
parseCNFLines = do
    lines <- many parseCNFLine
    eof
    return lines

parseCNFLine :: Parser CNFLine
parseCNFLine = do
    line <- choice [parseCellLine, 
                    parseSetPos,
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

-- SIGH
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

getRight :: Either a b -> b
getRight (Right x) = x
getRight _         = error "you suck"


{--
main = do
    putStrLn "Parsing object lines:"
    print $ parse parseCellLine "error" sampleCell1
    print $ parse parseCellLine "error" sampleCell2
    print $ parse parseCellLine "error" sampleCell3
    putStrLn "Parsing set lines:"
    print $ parse parseSetPos "error" sampleSet1
    print $ parse parseSetPos "error" sampleSet2
    print $ parse parseSetPos "error" sampleSet3
    putStrLn "Parsing a real set: "
    f <- readFile "aurora/aurora.cnf"
    let p = parse parseCNFLines "error" f
    let kissdata = either errorKiss linesToScript p
    print p
    putStrLn "all"
    print kissdata
--}
{--
 putStrLn "Number objects in data blob:"
    print $ length (kObjects kissdata)
    putStrLn "Number of sets in CNF: "
    let q = if isRight p then [ a | CNFSetPos a <- getRight p] else []
    print $ length q 
    --putStrLn "Set 1: "
    --print $ q !! 1
    putStrLn "Number positions in Set 1:"
    let r = map (\(KissSetPos palette positions) -> positions) q
    print $ length ( r !! 1)
    putStrLn "Number positions in 0, 1, 2,..."
    print $ length (head r)
    putStrLn "is set one the same as set 2"
    print $ zipWith (\x y -> x == y) (r !! 1) (r !! 2) 
    putStrLn "three and four?"
    print $ zipWith (\x y -> x == y) (r !! 3) (r !! 4) 
--}

