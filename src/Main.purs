module Main where

import Prelude

import Text.ICal (schedule)

import Text.Parsing.Parser (runParser)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Either (Either(..))


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  case runParser "RDATE;VALUE=DATE:19970304,19970504,19970704,19970904" schedule of

    Right results ->
      log $ show results

    Left err ->
      log $ show err
