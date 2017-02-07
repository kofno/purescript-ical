module Text.ICal.Calendar
  ( Calendar
  , calendarize
  )
  where

import Prelude
import Data.String (Pattern(..), split)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.List (List(..), filter, fromFoldable, length)

import Text.ICal.Content
  ( Content(..)
  , Param
  )


type CalendarRecord =
  { prodId :: ProdID
  , version :: ICalVersion
  , calendarScale :: Maybe CalScale
  , method :: Maybe Method
  , otherProperties :: List Content
  , vEvents :: List Content
  , vTodos :: List Content
  , vJournal :: List Content
  , vFreeBusy :: List Content
  , vTimezones :: List Content
  , vAlarms :: List Content
  , otherComponents :: List Content
  }

newtype Calendar =
  Calendar CalendarRecord


instance showCalendar :: Show Calendar where
  show (Calendar rec) =
    "Calendar { " -- TODO: full impl
    <> show (length rec.vEvents) <> " events"
    <> " }"

calendar :: ProdID
         -> ICalVersion
         -> Maybe CalScale
         -> Maybe Method
         -> List Content
         -> List Content
         -> List Content
         -> List Content
         -> List Content
         -> List Content
         -> List Content
         -> List Content
         -> CalendarRecord
calendar =
  { prodId: _
  , version: _
  , calendarScale: _
  , method: _
  , otherProperties: _
  , vEvents: _
  , vTodos: _
  , vJournal: _
  , vFreeBusy: _
  , vTimezones: _
  , vAlarms: _
  , otherComponents: _
  }


data ProdID
  = ProdID String (List Param)


data ICalVersion
  = ICalVersion String (List Param)
  | MinMaxICalVersion String String (List Param)


data CalScale
  = CalScale String (List Param)


data Method
  = Method String (List Param)


calendarize :: Content -> Either String Calendar
calendarize (Component "VCALENDAR" content) =
  let
    calrec =
      calendar
      <$> prodID content
      <*> iCalVersion content
      <*> calScale content
      <*> method content
      <*> otherProperties content
      <*> events content
      <*> todos content
      <*> journals content
      <*> freeBusy content
      <*> timezones content
      <*> alarms content
      <*> otherComponents content
  in
    Calendar <$> calrec
calendarize _ =
  Left "Expected a calendar component"


prodID :: List Content -> Either String ProdID
prodID contents =
  let
    makeProdID (ContentLine _ params v) =
      Right $ ProdID v params
    makeProdID _ =
      Left "Expected a content line with PRODID"
  in
    either Left makeProdID $ req1 "PRODID" contents


iCalVersion :: List Content -> Either String ICalVersion
iCalVersion content =
  let
    filtered = req1 "VERSION" content
  in
    case filtered of
      Right (ContentLine _ params v) ->
       parseVersion v params
      Right _ ->
        Left "Unexpected Content"
      Left msg ->
        Left msg


calScale :: List Content -> Either String (Maybe CalScale)
calScale content =
  case opt1 "CALSCALE" content of
    Right Nothing ->
      Right Nothing
    Right (Just (ContentLine _ params v)) ->
      Right $ Just $  CalScale v params
    Right _ ->
      Left "Unexepcted content"
    Left msg ->
      Left msg


method :: List Content -> Either String (Maybe Method)
method content =
  case opt1 "METHOD" content of
    Right Nothing ->
      Right Nothing
    Right (Just (ContentLine _ params v)) ->
      Right $ Just $ Method v params
    Right _ ->
      Left "Unexpected content"
    Left msg ->
      Left msg


otherProperty :: Content -> Boolean
otherProperty content =
  case content of
    (Component _ _) -> false
    (ContentLine "PRODID" _ _) -> false
    (ContentLine "CALSCALE" _ _) -> false
    (ContentLine "VERSION" _ _) -> false
    (ContentLine "METHOD" _ _) -> false
    _ -> true


otherProperties :: List Content -> Either String (List Content)
otherProperties = Right <<< filter otherProperty


isEvent :: Content -> Boolean
isEvent = isComponentOf "VEVENT"


events :: List Content -> Either String (List Content)
events = Right <<< filter isEvent


isTodo :: Content -> Boolean
isTodo = isComponentOf "VTODO"


todos :: List Content -> Either String (List Content)
todos = Right <<< filter isTodo


isJournal :: Content -> Boolean
isJournal = isComponentOf "VJOURNAL"


journals :: List Content -> Either String (List Content)
journals = Right <<< filter isJournal


isFreeBusy :: Content -> Boolean
isFreeBusy = isComponentOf "VFREEBUSY"


freeBusy :: List Content -> Either String (List Content)
freeBusy = Right <<< filter isFreeBusy


isTimezone :: Content -> Boolean
isTimezone = isComponentOf "VTIMEZONE"


timezones :: List Content -> Either String (List Content)
timezones = Right <<< filter isTimezone


isAlarm :: Content -> Boolean
isAlarm = isComponentOf "VALARM"


alarms :: List Content -> Either String (List Content)
alarms = Right <<< filter isAlarm


isOtherComponent :: Content -> Boolean
isOtherComponent (Component name _) =
  case name of
    "VEVENT" -> false
    "VTODO" -> false
    "VJOURNAL" -> false
    "VFREEBUSY" -> false
    "VTIMEZONE" -> false
    "VALARM" -> false
    _ -> true
isOtherComponent _ = false


otherComponents :: List Content -> Either String (List Content)
otherComponents = Right <<< filter isOtherComponent


isComponentOf :: String -> Content -> Boolean
isComponentOf name content =
  case content of
    (Component name' _) ->
      if name == name' then true
      else false
      
    _ -> false

req1 :: String -> List Content -> Either String Content
req1 name contents =
  case opt1 name contents of
    Right Nothing ->
      Left $ name <> " must occur at least once"
    Right (Just c) ->
      Right c
    Left msg ->
      Left msg


opt1 :: String -> List Content -> Either String (Maybe Content)
opt1 name contents =
  let
    filtered =
      filter isThing contents

    isThing thing =
      case thing of
        (ContentLine name' _ _) -> name == name'
        _ -> false
  in
    case filtered of
      (Cons cl Nil) ->
        Right $ Just cl
      Nil ->
        Right Nothing
      _ ->
        Left $ name <> " cannot appear more then one time: " <> show filtered


parseVersion :: String -> List Param -> Either String ICalVersion
parseVersion ver params =
  let
    parsed = fromFoldable $ split (Pattern ";") ver
  in
    case parsed of
      Nil ->
        Left "version can't be empty"
      (Cons v' Nil) ->
        Right $ ICalVersion v' params
      (Cons v1 (Cons v2 Nil)) ->
        Right $ MinMaxICalVersion v1 v2 params
      _ ->
        Left "version can only have a min and a max (too many parts)"
