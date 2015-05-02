{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ParseObjects where

import Text.ParserCombinators.Parsec
import Data.List

sampleCell1 :: String
sampleCell1 = "#19       markr6.cel      *0 : 0 1 2 3 4 5 6 7 8 9 &"
sampleCell2 :: String
sampleCell2 = "#25       tights1.cel     *0 : 0 1 2 3 4 5 6 7 8 9     ;%t75 &"
sampleCell3 :: String
sampleCell3 = "#180.99   handl.cel       *0 :   1     4     7 &"
sampleCells :: String
sampleCells = 
    "#1         markr6.cel          *0 : 0 1 2 3 4 5 6 7 8 9 &" ++
    "#24        shirt1.cel          *0 : 0 1 2 3 4 5 6 7 8 9 &" ++
    "#24        shirt1b.cel         *0 : 0 1 2 3 4 5 6 7 8 9 &" ++
    "#180.99    handl.cel           *0 :   1     4     7 "

data KissCell = KissCell {
                    celFix :: Int,
                    celFile :: String,
                    celPallette :: Int,
                    celSets :: [Int],
                    celTransp :: Int }
    deriving (Eq, Show)

data KissObject = KissObject {
                    objNum :: Int,
                    objCells :: [KissCell]}
    deriving (Eq, Show)

linesToObjects :: [(Int, KissCell)] -> [KissObject]
linesToObjects xs = 
    map (uncurry KissObject) objs
    where combine n xs = (n, map snd (filter (\x -> fst x == n) xs))
          objs = nub $ map (\x -> combine (fst x) xs) xs

parseObjects :: Parser [KissObject]
parseObjects = do 
    lines <- parseLine `sepBy` char '&'
    skipMany space
    return $ linesToObjects lines

parseLine :: Parser (Int, KissCell)
parseLine = do
    char '#'
    num <- many digit
    cell <- parseCell
    --skipMany space
    --char '&'
    return (read num, cell)

parseCell :: Parser KissCell
parseCell = do
    fix <- option 0 parseFix
    skipMany1 space
    cell <- many (noneOf " ")
    skipMany1 space
    char '*'
    pallette <- many digit
    skipMany space
    char ':'
    skipMany space
    sets <- option [] parseSets
    skipMany space
    transp <- option 0 parseTransp
    return $ KissCell fix cell (read pallette) sets transp

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

parseTransp :: Parser Int
parseTransp = do
    string ";%t"
    num <- many digit
    return $ read num

main = do
    print $ parse parseLine "error" sampleCell1
    print $ parse parseLine "error" sampleCell2
    print $ parse parseLine "error" sampleCell3
    print $ parse parseObjects "error" sampleCells
