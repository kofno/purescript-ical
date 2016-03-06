module Test.Main where

import Prelude

import Test.Unit (test, runTest, TIMER)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)

import Control.Monad.Eff (Eff)
import Control.Monad.Aff.AVar (AVAR)

import Data.Either (Either(..))
import Data.List (length)

import Text.ICal (schedule)
import Text.Parsing.Parser (runParser)

import Node.FS (FS)
import Node.FS.Aff as FS
import Node.Encoding (Encoding(..))

main :: forall e.
        Eff
          ( timer :: TIMER
          , avar :: AVAR
          , testOutput :: TESTOUTPUT
          , fs :: FS
          | e
          )
          Unit
main = runTest do
  test "parsing a single line" do
    case runParser singleLine schedule of
      Right result -> Assert.equal (length result) 1
      Left err -> Assert.assert (show err) false

  test "parsing multiple lines" do
    case runParser twoLines schedule of
      Right result -> Assert.equal (length result) 2
      Left err -> Assert.assert (show err) false

  test "parsing a folded line" do
    case runParser foldExample schedule of
      Right result -> Assert.equal (length result) 1
      Left err -> Assert.assert (show err) false

  test "iCal from a file" do
    contents <- FS.readTextFile UTF8 "US-Holidays.ics"
    case runParser contents schedule of
      Right result -> Assert.equal (length result) 456
      Left err -> Assert.assert (show err) false



singleLine :: String
singleLine = "RDATE;VALUE=DATE:19970304,19970504,19970704,19970904"


twoLines :: String
twoLines =
  "RDATE;VALUE=DATE:19970304,19970504,19970704,19970904\r\nATTENDEE;RSVP=TRUE:mailto:jsmith@example.com"


foldExample :: String
foldExample =
  "ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT:mailto:\r\n\tjsmith@example.com"