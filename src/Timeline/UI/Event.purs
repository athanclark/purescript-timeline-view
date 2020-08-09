module Timeline.UI.Event where

import Timeline.Time.Value (DecidedValue(..))
import Timeline.ID.Event (EventID(..))
import Timeline.ID.TimeSpace (TimeSpaceID (..))
import Timeline.ID.Timeline (TimelineID (..))
import Timeline.ID.ChildOrSiblingParent (ChildOrSiblingParentID(..))
import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , decodeJson
  , (:=)
  , (~>)
  , jsonEmptyObject
  , (.:)
  )
import Data.UUID (genUUID) as UUID
import Data.Default (class Default)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Random (randomBool)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)

-- | An event documented at time `index`.
-- |
-- | Defined over the user-level timescale `a`.
newtype Event
  = Event
  { name :: String
  , description :: String
  , id :: EventID
  , parent :: Maybe ChildOrSiblingParentID
  , time :: DecidedValue
  }

derive instance genericEvent :: Generic Event _

derive newtype instance eqEvent :: Eq Event

derive newtype instance showEvent :: Show Event

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (Event { name, description, id, parent, time }) =
    "name" := name
      ~> "description"
      := description
      ~> "id"
      := id
      ~> "parent"
      := parent
      ~> "time"
      := time
      ~> jsonEmptyObject

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    id <- o .: "id"
    parent <- o .: "parent"
    time <- o .: "time"
    pure (Event { name, description, id, parent, time })

instance arbitraryEvent :: Arbitrary Event where
  arbitrary = do
    name <- genString
    description <- genString
    id <- arbitrary
    parent <- arbitrary
    time <- arbitrary
    pure (Event { name, description, id, parent, time })

instance defaultEvent :: Default Event where
  def =
    Event
      { name: "Event"
      , description: ""
      , id: EventID (unsafePerformEffect UUID.genUUID)
      , parent: unsafePerformEffect do
        let genCorS = do
              cOrS <- randomBool
              ChildOrSiblingParentID <$>
                if cOrS
                  then Left <<< TimelineID <$> UUID.genUUID
                  else Right <<< TimeSpaceID <$> UUID.genUUID
        exists <- randomBool
        if exists then Just <$> genCorS else pure Nothing
      , time: DecidedValueNumber 0.0
      }
