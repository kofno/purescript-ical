module Text.ICal.Params where

import Prelude

import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (sepBy, optional)
import Text.Parsing.Parser.String (char, string)

import Text.ICal.Types
  ( Param(..)
  , CalendarUser(..)
  , Encoding(..)
  , FreeBusy(..)
  , ParticipationStatus(..)
  , TriggerState(..)
  , Relationship(..)
  , RoleType(..)
  , ValueType(..)
  )
import Text.ICal.Combinators (doubleQuotes, quoteSafeChars, safeChars, nameParser)

import Data.String (toUpper)
import Data.Maybe (Maybe(..))

import Control.Alt ((<|>))


param :: Parser String Param
param = do
  char ';'
  paramName <- nameParser
  char '='
  paramValue $ toUpper paramName


paramValueChars :: Parser String String
paramValueChars =
  doubleQuotes quoteSafeChars <|> safeChars


paramValue :: String -> Parser String Param
paramValue name =
  case name of

    -- https://tools.ietf.org/html/rfc5545#section-3.2.1
    "ALTREP" -> do
      -- TODO: Parse as a URI
      value <- doubleQuotes quoteSafeChars
      return $ Altrep value

    -- https://tools.ietf.org/html/rfc5545#section-3.2.2
    "CN" -> do
      value <- paramValueChars
      return $ CommonName value

    -- https://tools.ietf.org/html/rfc5545#section-3.2.3
    "CUTYPE" -> do
      value <- paramValueChars
      return $
        case value of
          "INDIVIDUAL" ->
            CalendarUserType Individual
          "GROUP" ->
            CalendarUserType Group
          "RESOURCE" ->
            CalendarUserType Resource
          "ROOM" ->
            CalendarUserType Room
          "UNKNOWN" ->
            CalendarUserType (Unknown Nothing)
          _ ->
            CalendarUserType (Unknown $ Just value)

    -- https://tools.ietf.org/html/rfc5545#section-3.2.4
    "DELEGATED-FROM" -> do
      values <- doubleQuotes quoteSafeChars `sepBy` char ','
      return $ DelegatedFrom values

    -- https://tools.ietf.org/html/rfc5545#section-3.2.5
    "DELEGATED-TO" -> do
      values <- doubleQuotes quoteSafeChars `sepBy` char ','
      return $ DelegatedTo values

    -- https://tools.ietf.org/html/rfc5545#section-3.2.6
    "DIR" -> do
      value <- doubleQuotes quoteSafeChars
      return $ Dir value

    -- https://tools.ietf.org/html/rfc5545#section-3.2.7
    "ENCODING" -> do
      value <- string "8BIT" <|> string "BASE64"
      case value of
        "8BIT" ->
          return $ EncodingType EightBit
        "BASE64" ->
          return $ EncodingType Base64
        _ ->
          fail $ "Unrecognized encoding: " ++ value

    -- https://tools.ietf.org/html/rfc5545#section-3.2.8
    "FMTTYPE" -> do
      -- TODO: Properly parse type
      --   https://tools.ietf.org/html/rfc4288#section-4.2
      value <- paramValueChars
      return $ FormatType value

    -- https://tools.ietf.org/html/rfc5545#section-3.2.9
    "FBTYPE" -> do
      value <- nameParser
      return $
        case value of
          "FREE" ->
            FreeBusyType Free
          "BUSY" ->
            FreeBusyType (Busy Nothing)
          "BUSY-TENTATIVE" ->
            FreeBusyType BusyTentative
          "BUSY-UNAVAILABLE" ->
            FreeBusyType BusyUnavailable
          _ ->
            FreeBusyType (Busy (Just value))

    -- https://tools.ietf.org/html/rfc5545#section-3.2.10
    "LANGUAGE" -> do
      -- TODO: Parse language tag: https://tools.ietf.org/html/rfc5646
      value <- safeChars
      return $ Language value

    -- https://tools.ietf.org/html/rfc5545#section-3.2.11
    "MEMBER" -> do
      values <- doubleQuotes quoteSafeChars `sepBy` char ','
      return $ Member values

    -- https://tools.ietf.org/html/rfc5545#section-3.2.12
    "PARTSTAT" -> do
      value <- nameParser
      return $
        case value of
          "NEEDS-ACTION" ->
            PartStat (NeedsAction Nothing)
          "ACCEPTED" ->
            PartStat Accepted
          "DECLINED" ->
            PartStat Declined
          "TENTATIVE" ->
            PartStat Tentative
          "DELEGATED" ->
            PartStat Delegated
          "IN-PROCESS" ->
            PartStat InProcess
          "COMPLETED" ->
            PartStat Completed
          _ ->
            PartStat (NeedsAction (Just value))

    -- https://tools.ietf.org/html/rfc5545#section-3.2.13
    "RANGE" -> do
      value <- string "THISANDFUTURE"
      return $ Range value

    -- https://tools.ietf.org/html/rfc5545#section-3.2.14
    "RELATED" -> do
      value <- string "START" <|> string "END"
      return $
        case value of
          "START" ->
            TriggerRelation Start
          "END" ->
            TriggerRelation End
          _ ->
            TriggerRelation Start

    -- https://tools.ietf.org/html/rfc5545#section-3.2.15
    "RELTYPE" -> do
      value <- nameParser
      return $ RelType $
        case value of
          "Parent" ->
            Parent Nothing
          "CHILD" ->
            Child
          "SIBLING" ->
            Sibling
          _ ->
            Parent (Just value)

    -- https://tools.ietf.org/html/rfc5545#section-3.2.16
    "ROLE" -> do
      value <- nameParser
      return $ Role $
        case value of
          "CHAIR" ->
            Chair
          "REQ-PARTICIPANT" ->
            RequiredParticipant Nothing
          "OPT-PARTICIPANT" ->
            OptionalParticipant
          "NON-PARTICIPANT" ->
            NonParticipant
          _ ->
            RequiredParticipant $ Just value

    -- https://tools.ietf.org/html/rfc5545#section-3.2.17
    "RSVP" -> do
      value <- string "TRUE" <|> string "FALSE"
      return $ RSVP $
        case value of
          "TRUE" -> true
          "FALSE" -> false
          _ -> false

    -- https://tools.ietf.org/html/rfc5545#section-3.2.18
    "SENT-BY" -> do
      value <- doubleQuotes quoteSafeChars
      return $ SentBy value

    -- https://tools.ietf.org/html/rfc5545#section-3.2.19
    "TZID" -> do
      optional (char '/')
      value <- paramValueChars
      return $ TimezoneID value

    -- https://tools.ietf.org/html/rfc5545#section-3.2.20
    "VALUE" -> do
      value <- nameParser
      return $ Value $
        case value of
          "BINARY" ->
            BinaryValue
          "BOOLEAN" ->
            BooleanValue
          "CAL-ADDRESS" ->
            CalendarAddress
          "DATE" ->
            DateValue
          "DATE-TIME" ->
            DateTimeValue
          "DURATION" ->
            DurationValue
          "FLOAT" ->
            FloatValue
          "INTEGER" ->
            IntegerValue
          "PERIOD" ->
            PeriodValue
          "RECUR" ->
            RecurValue
          "TEXT" ->
            TextValue
          "TIME" ->
            TimeValue
          "URI" ->
            UriValue
          "UTC-OFFSET" ->
            UtcOffsetValue
          _ ->
            OtherValue value

    _ -> do
      values <- paramValueChars `sepBy` char ','
      return $ OtherParam name values
