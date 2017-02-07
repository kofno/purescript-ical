module Text.ICal.Combinators where

import Prelude

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.String (string, char, noneOf)
import Text.Parsing.Parser.Token (alphaNum)

import Data.Char (fromCharCode)
import Data.String (fromCharArray, toCharArray)
import Data.Array (range, singleton, some, many)

import Control.Alt ((<|>))



crlf :: Parser String String
crlf =
  string "\r\n"


nameParser :: Parser String String
nameParser =
  xName <|> unfold ianaToken


xName :: Parser String String
xName = do
  char 'X'
  char '-'
  rest <- ianaToken
  pure ("X-" <> rest)


ianaChar :: Parser String Char
ianaChar =
  alphaNum <|> char '-'


ianaToken :: Parser String String
ianaToken =
  fromCharArray <$> some (unfold ianaChar)


valueParser :: Parser String String
valueParser =
  fromCharArray <$> many (unfold (noneOf nonTextual))


doubleQuotes :: Parser String String -> Parser String String
doubleQuotes =
  P.between (string "\"") (string "\"")


quoteSafeChars :: Parser String String
quoteSafeChars = fromCharArray <$> many (unfold (noneOf $ controls <> (singleton '"')))


safeChars :: Parser String String
safeChars = fromCharArray <$> many (unfold (noneOf $ controls <> toCharArray "\";:,"))


unfold :: forall a. Parser String a -> Parser String a
unfold =
  P.between (P.optional fws) (P.optional fws)


fws :: Parser String String
fws =
  string "\r\n " <|> string "\r\n\t"


controls :: Array Char
controls =
  fromCharCode <$> (range 0x00 0x08) <> (range 0x0A 0x1F) <> (singleton 0x7F)


nonTextual :: Array Char
nonTextual =
  fromCharCode <$> (range 0x00 0x08) <> (range 0x0A 0x1F) <> (range 0x7F 0x9F) <> (range 0xA1 0xFF)
