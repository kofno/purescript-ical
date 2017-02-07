module Main where

import Prelude
import Control.Monad.Aff (Canceler, launchAff)
import Text.Parsing.Parser (runParser)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Either (Either(..))
import Node.FS (FS)
import Node.FS.Aff as FS
import Node.Encoding (Encoding(..))
import Text.ICal (schedule)


main :: forall e.
        Eff (console :: CONSOLE, fs :: FS, err :: EXCEPTION | e)
        (Canceler (fs :: FS, console :: CONSOLE | e))
main = launchAff $ do
  contents <- FS.readTextFile UTF8 "US-Holidays.ics"
  case runParser contents schedule of

    Right results ->
      liftEff $ log $ show results

    Left err ->
      liftEff $ log $ show err
