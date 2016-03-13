module Text.ICal where

import Prelude

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (sepEndBy)
import Text.Parsing.Parser.String (eof, char)

import Data.List (List(..), (:), many, span, drop)

import Text.ICal.Types (Schedule, Content(..))
import Text.ICal.Combinators (crlf, nameParser, valueParser)
import Text.ICal.Params (param)


schedule :: Parser String Schedule
schedule = do
  result <- contentLine `sepEndBy` crlf
  eof
  return $ componentize result


contentLine :: Parser String Content
contentLine = do
  lineName <- nameParser
  params <- many param
  char ':'
  result <- valueParser
  return $
    ContentLine lineName params result


componentize :: List Content -> List Content
componentize Nil = Nil
componentize (Cons cl@(ContentLine "BEGIN" _ v) rest) =
  let
    nextComponent =
      span tilEnd rest

    tilEnd (ContentLine "END" _ v')
      | v == v' = false
      | otherwise = true
    tilEnd _ =
      true

    comp = nextComponent.init
    rest' = drop 1 nextComponent.rest
  in
    Component v (componentize comp) : componentize rest'
componentize (Cons first rest) =
  first : componentize rest
