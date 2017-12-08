module JsonParser where

import Data.Void
import Control.Applicative
import Text.Megaparsec
import  Text.Megaparsec.Char as C
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map

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
sc :: Parser ()
sc = L.space C.space1 empty empty

-- Parser that automatically consumes white space. As well as some other thing
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbol parser that automatically gets rid of white space
symbol :: String -> Parser String
symbol = L.symbol sc

-- Parse String to basic types
parseNull :: Parser ()
parseNull = do string "null" ; return ()

parseBool :: Parser Bool
parseBool = try (do string "true"  ; return True) <|>
                (do string "false" ; return False)

parseNum :: Parser Double
parseNum = L.signed sc (lexeme L.float)

parseStr :: Parser String
parseStr = do
  _ <- lexeme $ char '\"'
  parseStringChar `manyTill` (char '\"')
  where
    parseStringChar =
      (do
          _ <- char '\\'
          c <- anyChar
          case c of
            '\"' -> return '\"'
            '\\' -> return '\\'
            'b' -> return '\b'
            'f' -> return '\f'
            'n' -> return '\n'
            'r' -> return '\r'
            't' -> return '\t'
            'v' -> return '\v'
            )
      <|> anyChar

parseArr :: Parser [JSON]
parseArr = do
  _ <- lexeme $ char '['
  arr <- parseJSON `sepBy` (lexeme $ char ',')
  _ <- lexeme $ char ']'
  return arr

parseMap :: Parser (Map.Map String JSON)
parseMap = do
  _ <- lexeme $ char '{'
  m <- keyValPair `sepBy` char ','
  _ <- lexeme $ char '}'
  return $ Map.fromList m
  where
    keyValPair = do
      _ <- many sc
      key <- parseStr
      _ <- lexeme $ char ':'
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
parseJsonStr = fmap JsonString parseStr

parseJsonArr :: Parser JSON
parseJsonArr = fmap JsonArr parseArr

parseJsonMap :: Parser JSON
parseJsonMap = fmap JsonMap parseMap

-- Main parser
parseJSON :: Parser JSON
parseJSON = do
  _ <- many sc -- Parse white space before JSON
  json <- parseJsonNull
      <|> parseJsonBool
      <|> parseJsonNum
      <|> parseJsonStr
      <|> parseJsonArr
      <|> parseJsonMap
  _ <- many sc
  return json
