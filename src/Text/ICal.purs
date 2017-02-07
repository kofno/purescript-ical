module Text.ICal
  ( Schedule
  , schedule
  )
  where

import Prelude

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (sepEndBy)
import Text.Parsing.Parser.String (eof, char)

import Data.List (List(..), (:), many, span, drop)
import Data.Either (Either)

import Text.ICal.Content (Content(..))
import Text.ICal.Combinators (crlf, nameParser, valueParser)
import Text.ICal.Params (param)
import Text.ICal.Calendar (Calendar, calendarize)


type Schedule
  = List (Either String Calendar)


schedule :: Parser String Schedule
schedule = do
  content <- contentLine `sepEndBy` crlf
  eof
  pure $ calendarize <$> componentize content


contentLine :: Parser String Content
contentLine = do
  lineName <- nameParser
  params <- many param
  char ':'
  result <- valueParser
  pure $
    ContentLine lineName params result


componentize :: List Content -> List Content
componentize Nil = Nil
componentize (Cons (ContentLine "BEGIN" _ v) rest) =
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
