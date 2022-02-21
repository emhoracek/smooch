{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module ParseCNF where

import           Control.Monad.Trans.Except
import           Data.Either
import           Data.Array                    ((!))
import qualified Data.Array                    as A
import           Data.Char                     (toLower, isSpace)
import           Data.List                     hiding (lines)
import           Data.Maybe
import qualified Data.Text                     as T
import           Kiss
import           Text.ParserCombinators.Parsec

getKissSet :: String -> ExceptT T.Text IO KissSet
getKissSet file = do
  kissData <- getKissData file
  kissCels <- getKissCels file
  kissPalettes <- getKissPalettes kissData
  return $ KissSet kissData kissCels kissPalettes

getKissData :: String -> ExceptT T.Text IO CNFKissData
getKissData file =
  case parse parseCNFLines "KiSS CNF error: " (map toLower file) of
    Right ls -> return $ linesToScript ls
    Left  e  -> throwE $ T.pack $ show e

getKissCels :: String -> ExceptT T.Text IO [CNFKissCel]
getKissCels file =
  case parse parseCNFLines "KiSS cel error: " (map toLower file) of
    Right ls -> return $ linesToCels ls
    Left  e  -> throwE $ T.pack $ show e

getKissPalettes :: CNFKissData -> ExceptT T.Text IO Palettes
getKissPalettes file = return $ toArray (cnfkPalettes file)
  where toArray l = A.listArray (0, length l - 1) l

lookupPalette :: Int -> Palettes -> ExceptT T.Text IO PaletteFilename
lookupPalette n palettes =
  if snd (A.bounds palettes) >= n
    then return (palettes ! n)
    else throwE "Palette not found"

defaultPalette :: Palettes -> ExceptT T.Text IO PaletteFilename
defaultPalette = lookupPalette 0

-- Converting CNF lines to useable KiSS data
linesToScript :: [CNFLine] -> CNFKissData
linesToScript xs =
    CNFKissData memory border palettes windowSize cels positions
    where -- memory and border are optional
          memory   = fromMaybe 0 (listToMaybe [ a | CNFMemory a <- xs ])
          border   = fromMaybe 0 (listToMaybe [ a | CNFBorder a <- xs ])
          -- at least one palette must be specified
          palettes = [ a | CNFPalette a <- xs ]
          -- window is (600, 480) by default
          windowSize = fromMaybe (600, 480) (listToMaybe [ a | CNFWindowSize a <- xs ])
          -- turn object #, cel, set positions to objects
          positions = [ a | CNFSetPos a <- xs ]
          cels = [ cnfToKissCel (0,0) a | CNFCel a <- xs ]

cnfToKissCel :: (Int, Int) -> CNFKissCel ->  KissCel
cnfToKissCel (xoff, yoff) CNFKissCel{..} =
  KissCel cnfCelMark cnfCelFix cnfCelName cnfCelPalette cnfCelSets cnfCelAlpha (Position xoff yoff)

linesToCels :: [CNFLine] -> [CNFKissCel]
linesToCels xs = [ a | CNFCel a <- xs ]

-- KiSS Parser Combinators

-- Parses the lines describing cels and objects.
parseCelLine :: Parser CNFLine
parseCelLine = do
    char '#'
    cel <- parseCel
    optional parseComment
    skipMany digit
    return $ CNFCel cel

parseCel :: Parser CNFKissCel
parseCel = do
    mark <- many1 digit
    fix <- option 0 parseFix
    skipMany1 space
    file <- many1 (noneOf ". ")
    string ".cel" <?> "cel file extenstion"
    skipMany1 space
    palette <- option 0 (try parseCelPalette)
    skipMany space
    sets <- option [0..9] parseSets
    skipMany space
    transp <- option 0 (try parseTransp)
    return $ CNFKissCel (read mark) fix file palette sets transp

{--
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""--}

parseCelPalette :: Parser Int
parseCelPalette = do
    char '*'
    num <- many1 digit
    return $ read num

parseFix :: Parser Int
parseFix = do
    char '.'
    num <- option "0" (try $ many1 digit)
    return $ read num

-- This parses all the list of sets the cel will be displayed in
parseSets :: Parser [Int]
parseSets = do
  char ':'
  skipMany space
  many parseSet

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
    num <- option "0" (many1 digit)
    return $ CNFBorder (read num)

-- The following four functions parse the cel positions for each set of cels.
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
    xsign <- option '0' (char '-')
    xpos <- many1 digit
    char ','
    ysign <- option '0' (char '-')
    ypos <- many1 digit
    return $ Position (read $ xsign : xpos) (read $ ysign : ypos)

parseNoPos :: Parser SetPos
parseNoPos = do
    char '*'
    return NoPosition

data CNFLine = CNFMemory Int
             | CNFBorder Int
             | CNFPalette String
             | CNFWindowSize (Int, Int)
             | CNFCel CNFKissCel
             | CNFSetPos KissSetPos
             | CNFComment String
             | CNFJunkLine
    deriving (Eq, Show)

-- Parse the CNF one "line" (including multi-line set descriptions) at a time
parseCNFLines :: Parser [CNFLine]
parseCNFLines = do
    ls <- many parseCNFLine
    optional (char '\FS')
    eof
    return ls

parseCNFLine :: Parser CNFLine
parseCNFLine = do
    line <- choice [parseCelLine,
                    parseSetPos,
                    parseMemory, parsePalette,
                    parseBorder, parseWindowSize,
                    parseCNFComment,
                    parseCNFJunk]
    spaces
    return line

parseCNFJunk :: Parser CNFLine
parseCNFJunk = do
    char '\SUB'
    return CNFJunkLine

parseCNFComment :: Parser CNFLine
parseCNFComment = do
    char ';'
    comment <- many (noneOf "\r\n")
    return $ CNFComment comment
