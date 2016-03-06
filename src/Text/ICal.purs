module Text.ICal where

import Prelude

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (sepEndBy)
import Text.Parsing.Parser.String (eof, char)

import Data.Array (many)

import Text.ICal.Types (Schedule, ContentLine(..))
import Text.ICal.Combinators (crlf, nameParser, valueParser)
import Text.ICal.Params (param)


schedule :: Parser String Schedule
schedule = do
  result <- contentLine `sepEndBy` crlf
  eof
  return result


contentLine :: Parser String ContentLine
contentLine = do
  lineName <- nameParser
  params <- many param
  char ':'
  result <- valueParser
  return $ ContentLine
    { name : lineName
    , params : params
    , value : result
    }
