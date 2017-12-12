module JsonParser where

import Data.Void
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map
import Data.Char (isPrint)

{-
  object
    {}
    { members }
  members
    pair
    pair , members
  pair
    string : value
  array
    []
    [ elements ]
  elements
    value
    value , elements
  value
    string
    number
    object
    array
    true
    false
    null
-}


data JSON = JsonNull
            | JsonNum Double
            | JsonBool Bool
            | JsonString String
            | JsonArr [JSON]
            | JsonMap (Map.Map String JSON)
            deriving (Eq, Ord, Show)

type Parser = Parsec Void String

-- Space consumer. Since JSON does not have comments, both line and block comments are empty
unpad :: Parser a -> Parser a
unpad p = space *> p <* space

-- Symbol parser that automatically gets rid of white space
symbol :: String -> Parser String
symbol = unpad . string

-- Parse String to basic types
parseNull :: Parser ()
parseNull = do
  _ <- string "null"
  return ()

parseBool :: Parser Bool
parseBool = try (do string "true" ; return True) <|>
                (do string "false" ; return False)

parseNum :: Parser Double
parseNum = L.signed space L.float

escapedQuote :: Parser Char
escapedQuote = escapedSymbol (string "\"" >> return '\"')

escapedNewLine :: Parser Char
escapedNewLine = escapedSymbol (char 'n')

escapedTab :: Parser Char
escapedTab = escapedSymbol (char 't')

escapedSymbol :: Parser Char -> Parser Char
escapedSymbol p = string "\\" *> p

escapedSlash :: Parser Char
escapedSlash = string "\\\\" >> return '\\'

parseSafeChar :: Parser Char
parseSafeChar = try escapedQuote <|> try escapedTab <|> try escapedNewLine <|> try escapedSlash <|> try (satisfy ( \x ->  x /=  '\"' && isPrint x))

parseSafeStr :: Parser String
parseSafeStr =  char '\"' *> many parseSafeChar <* char '\"'

parseArr :: Parser [JSON]
parseArr = do
  _ <- unpad $ char '['
  arr <- parseJSON `sepBy` (unpad $ char ',')
  _ <- unpad $ char ']'
  return arr

parseMap :: Parser (Map.Map String JSON)
parseMap = do
  _ <- unpad $ char '{'
  m <- keyValPair `sepBy` char ','
  _ <- unpad $ char '}'
  return $ Map.fromList m
  where
    keyValPair = do
      _ <- space
      key <- parseSafeStr
      _ <- unpad $ char ':'
      val <- parseJSON
      return (key, val)

-- Transform basic Types to corresponding JSON datatype
parseJsonNull :: Parser JSON
parseJsonNull = do
  _ <- parseNull
  return JsonNull

parseJsonBool :: Parser JSON
parseJsonBool = fmap JsonBool parseBool

parseJsonNum :: Parser JSON
parseJsonNum = fmap JsonNum parseNum

parseJsonStr :: Parser JSON
parseJsonStr = fmap JsonString parseSafeStr

parseJsonArr :: Parser JSON
parseJsonArr = fmap JsonArr parseArr

parseJsonMap :: Parser JSON
parseJsonMap = fmap JsonMap parseMap

parseJSON :: Parser JSON
parseJSON = parseJsonNull
        <|> parseJsonBool
        <|> parseJsonNum
        <|> parseJsonStr
        <|> parseJsonArr
        <|> parseJsonMap
-- Main parser
parseJSONTop :: Parser JSON
parseJSONTop = do
  _ <- space -- Parse white space before JSON
  json <- parseJSON
  _ <- space
  _ <- eof  -- Parse white space after JSON
  return json
