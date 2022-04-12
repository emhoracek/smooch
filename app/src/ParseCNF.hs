{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module ParseCNF where

import           Control.Monad.Trans.Except    (ExceptT, throwE)
import           Data.Array                    ((!))
import qualified Data.Array                    as A
import           Data.Char                     (toLower)
import           Data.Maybe                    (fromMaybe, listToMaybe )
import qualified Data.Text                     as T
import           Text.ParserCombinators.Parsec

import           Kiss

getKissDoll :: String -> ExceptT T.Text IO KissDoll
getKissDoll file = do
  kissData <- getKissData file
  kissCels <- getKissCels file
  kissPalettes <- getKissPalettes kissData
  return $ KissDoll kissData kissCels kissPalettes

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
    CNFKissData memory border palettes windowSize cels positions fkiss
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
          fkiss = [ a | CNFFKiSSEvent a <- xs ]

cnfToKissCel :: (Int, Int) -> CNFKissCel ->  KissCel
cnfToKissCel (xoff, yoff) CNFKissCel{..} =
  KissCel cnfCelMark cnfCelFix cnfCelName cnfCelPalette cnfCelSets cnfCelAlpha (Position xoff yoff)

linesToCels :: [CNFLine] -> [CNFKissCel]
linesToCels xs = [ a | CNFCel a <- xs ]

-- KiSS Parser Combinators

spaceOrTab :: Parser Char
spaceOrTab = oneOf " \t\r"

spacesOrTabs :: Parser ()
spacesOrTabs = skipMany spaceOrTab

-- Parses the lines describing cels and objects.
parseCelLine :: Parser CNFLine
parseCelLine = do
    char '#'
    cel <- parseCel
    skipMany (noneOf "\n")
    return $ CNFCel cel

parseCel :: Parser CNFKissCel
parseCel = do
    mark <- many1 digit
    fix <- option 0 parseFix
    skipMany1 spaceOrTab
    file <- many1 (noneOf ". ")
    string ".cel" <?> "cel file extension"
    skipMany spaceOrTab
    palette <- option 0 (try parseCelPalette)
    spacesOrTabs
    sets <- option [0..9] parseSets
    spacesOrTabs
    transp <- option 0 (try parseTransp)
    return $ CNFKissCel (read mark) fix file palette sets transp

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
  spacesOrTabs
  many parseSet

parseSet :: Parser Int
parseSet = do
    num <- digit
    spacesOrTabs
    return $ read [num]

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
    skipMany (noneOf "\n")
    return $ CNFWindowSize (read width, read height)

parsePalette :: Parser CNFLine
parsePalette = do
    char '%'
    filename <- many (choice [letter, digit, char '_', char '-', char '.'])
    skipMany (noneOf "\n")
    return $ CNFPalette filename

parseBorder :: Parser CNFLine
parseBorder = do
    char '['
    num <- option "0" (many1 digit)
    skipMany (noneOf "\n")
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
    spacesOrTabs
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
             | CNFFKiSSEvent FKiSSEvent
             | CNFJunkLine
    deriving (Eq, Show)

-- Parse the CNF one "line" (including multi-line set descriptions) at a time
parseCNFLines :: Parser [CNFLine]
parseCNFLines = do
    ls <- many1 parseCNFLine
    eof
    return ls

parseCNFLine :: Parser CNFLine
parseCNFLine = do
    line <- choice [ parseCelLine, parseSetPos, parseCNFComment,
                     parsePalette, parseBorder, parseWindowSize,
                     parseEventHandler, parseFKiSSLine, parseMemory,
                     parseCNFJunk, parseEmptyLine ]
    spaces
    return line

parseEventHandler :: Parser CNFLine
parseEventHandler = try $ do
    string ";@eventhandler" <?> "\";@EventHandler\""
    skipMany (noneOf ";")
    return CNFJunkLine

parseFKiSSLine :: Parser CNFLine
parseFKiSSLine = do
    string ";@"
    eventName <- many1 alphaNum <?> "event name"
    char '('
    args <- parseFKiSSArg `sepBy` (char ',' >> spaces)
    char ')'
    spacesOrTabs
    optional parseCNFComment
    optional newline
    commands <- concat <$> many parseFKiSSActionLine
    optional parseCNFComment
    return $ CNFFKiSSEvent (FKiSSEvent eventName args commands)

parseFKiSSActionLine :: Parser [FKiSSAction]
parseFKiSSActionLine = try $ do
    optional $ do string ";@"
                  many1 space
    actions <- many1 parseFKiSSAction
    optional newline
    optional parseCNFComment
    return actions

parseFKiSSAction :: Parser FKiSSAction
parseFKiSSAction = do
    actionName <- many1 alphaNum <?> "action name"
    char '('
    args <- parseFKiSSArg `sepBy` (char ',' >> spaces)
    char ')'
    spacesOrTabs
    optional newline
    optional parseCNFComment
    return $ FKiSSAction actionName args

parseFKiSSArg :: Parser FKiSSArg
parseFKiSSArg = parseObj <|> parseString <|> parseNumber
    where parseObj = do
            char '#'
            Object . read <$> many1 digit
          parseString = do
            char '"'
            str <- many (noneOf "\"")
            char '"'
            return (Text str)
          parseNumber = do
            minus <- optionMaybe (char '-')
            n <- many1 digit
            return $ Number $ read (maybe n (\_ -> '-': n) minus)

parseEmptyLine :: Parser CNFLine
parseEmptyLine = try $ do
    optional (char '\r') >> newline <?> "empty line"
    return CNFJunkLine

parseCNFJunk :: Parser CNFLine
parseCNFJunk = do
    oneOf "\SUB\FS \t\r" <?> "whitespace"
    return CNFJunkLine

parseCNFComment :: Parser CNFLine
parseCNFComment = try $ do
    char ';'
    notFollowedBy (char '@')
    comment <- many (noneOf "\n")
    return $ CNFComment comment
