module Modules.ConfigReader (
  parseConfig
) where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language


-- Configuration file parser
configFile :: GenParser Char st [[String]]
configFile = do
  skipMany configComment
  sepEndBy configOption (char '\n' >> skipMany configComment)

configComment :: GenParser Char st [String]
configComment = do
  char '#'
  many (noneOf "\n")
  eol
  return []

configOption :: GenParser Char st [String]
configOption = do
  head <- optHead
  sep  <- optDefSeperate
  def  <- try optDefs <|> optDef
  return (head:def)

optHead :: GenParser Char st String
optHead = do
  def <- try (string "WindRiverPath") <|>
         try (string "TempDirPath")   <|>
         try (string "MrAccessApi")   <|>
         try (string "Projects")
  return def

optDefSeperate :: GenParser Char st String
optDefSeperate = do
  skipMany (try mySpace)
  string ":"
  skipMany (try mySpace)
  return ":"

optDef :: GenParser Char st [String]
optDef = do
  def <- many (noneOf "\n:")
  return (def:[])

optDefs :: GenParser Char st [String]
optDefs = do
  eol
  optDefPrefix
  x <- many (noneOf ",\n:")
  xs <- try optDefs <|> return []
  return (x:xs)

eol :: GenParser Char st Char
eol = try (char '\n') <|>
      fail "Error: Can't find end of line"

mySpace :: GenParser Char st Char
mySpace = do
  x <- char ' ' <|> char '\t'
  return x

optDefPrefix :: GenParser Char st String
optDefPrefix = string "- "

parseConfig :: String -> Either ParseError [[String]]
parseConfig input = parse configFile "(unknown)" input
