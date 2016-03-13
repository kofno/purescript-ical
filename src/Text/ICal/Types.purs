module Text.ICal.Types where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe)
import Data.Generic (class Generic, gShow)
import Data.Foldable (intercalate)


type Schedule
  = List Content


type Name = String
type Value = String


data Content
  = ContentLine Name (List Param) Value
  | Component Name (List Content)

instance showContent :: Show Content where
  show (ContentLine name params value) =
    "ContentLine: " ++ name ++ " " ++ show params ++ " " ++ value

  show (Component name contents) =
    "Component: " ++ name ++ "\n" ++ show contents


data Param
  = Altrep String
  | CommonName String
  | CalendarUserType CalendarUser
  | DelegatedFrom (List String)
  | DelegatedTo (List String)
  | Dir String
  | EncodingType Encoding
  | FormatType String
  | FreeBusyType FreeBusy
  | Language String
  | Member (List String)
  | PartStat ParticipationStatus
  | Range String
  | TriggerRelation TriggerState
  | RelType Relationship
  | Role RoleType
  | RSVP Boolean
  | SentBy String
  | TimezoneID String
  | Value ValueType
  | OtherParam String (List String)

instance showParam :: Show Param where
  show (Altrep s) =
    "Altrep:" ++ s

  show (CommonName s) =
    "CN:" ++ s

  show (CalendarUserType cu) =
    "CUTYPE:" ++ show cu

  show (DelegatedFrom ss) =
    "DELEGATED-FROM:" ++ intercalate "," ss

  show (DelegatedTo ss) =
    "DELEGATED-TO:" ++ intercalate "," ss

  show (Dir s) =
    "DIR:" ++ s

  show (EncodingType enctype) =
    "ENCODING:" ++ show enctype

  show (FormatType s) =
    "FMTTYPE:" ++ s

  show (FreeBusyType fbtype) =
    "FBTYPE:" ++ show fbtype

  show (Language s) =
    "LANGUAGE:" ++ s

  show (Member ss) =
    "MEMBER:" ++ intercalate "," ss

  show (PartStat status) =
    "PARTSTAT:" ++ show status

  show (Range s) =
    "RANGE:" ++ s

  show (TriggerRelation state) =
    "RELATED:" ++ show state

  show (RelType relationship) =
    "RELTYPE:" ++ show relationship

  show (Role roleType) =
    "ROLE:" ++ show roleType

  show (RSVP rsvp) =
    if rsvp
      then "RSVP TRUE"
      else "RSVP FALSE"

  show (SentBy s) =
    "SENT-BY:" ++ s

  show (TimezoneID s) =
    "TZID:" ++ s

  show (Value valueType) =
    "VALUE:" ++ show valueType

  show (OtherParam s ss) =
    "OtherParam:" ++ s ++ " " ++ intercalate "," ss


data CalendarUser
  = Individual
  | Group
  | Resource
  | Room
  | Unknown (Maybe String)

derive instance genericCalendarUser :: Generic CalendarUser

instance showCalendarUser :: Show CalendarUser where
  show = gShow


data Encoding
  = EightBit
  | Base64

derive instance genericEncoding :: Generic Encoding

instance showEncoding :: Show Encoding where
  show = gShow


data FreeBusy
  = Free
  | Busy (Maybe String)
  | BusyUnavailable
  | BusyTentative

derive instance genericFreeBusy :: Generic FreeBusy

instance showFreeBusy :: Show FreeBusy where
  show = gShow


data ParticipationStatus
  = NeedsAction (Maybe String)
  | Accepted
  | Declined
  | Tentative
  | Delegated
  | InProcess
  | Completed

derive instance genericParticipationStatus :: Generic ParticipationStatus

instance showParticipationStatus :: Show ParticipationStatus where
  show = gShow


data TriggerState
  = Start
  | End

derive instance genericTriggerState :: Generic TriggerState

instance showTriggerState :: Show TriggerState where
  show = gShow


data Relationship
  = Parent (Maybe String)
  | Child
  | Sibling

derive instance genericRelationship :: Generic Relationship

instance showRelationship :: Show Relationship where
  show = gShow


data RoleType
  = Chair
  | RequiredParticipant (Maybe String)
  | OptionalParticipant
  | NonParticipant

derive instance genericRoleType :: Generic RoleType

instance showRoleType :: Show RoleType where
  show = gShow


data ValueType
  = BinaryValue
  | BooleanValue
  | CalendarAddress
  | DateValue
  | DateTimeValue
  | TimeValue
  | DurationValue
  | FloatValue
  | IntegerValue
  | PeriodValue
  | RecurValue
  | TextValue
  | UriValue
  | UtcOffsetValue
  | OtherValue String

derive instance genericValueType :: Generic ValueType

instance showValueType :: Show ValueType where
  show = gShow
