module Timeline.UI.TimeSpace where

import Timeline.UI.TimeSpace.TimeScale (TimeScale)
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly)
import Timeline.ID.TimeSpace (TimeSpaceID(..))
import Timeline.ID.Timeline (TimelineID)
import Timeline.ID.Event (EventID)
import Timeline.ID.TimeSpan (TimeSpanID)
import Timeline.ID.RootTimeSpaceOrParent (RootTimeSpaceOrParentID)
import Prelude
import Data.Maybe (Maybe (..))
import Data.Default (def)
import Data.UUID (genUUID) as UUID
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
import Data.Array.Unique (UniqueArray)
import Data.Array.Unique (empty) as UniqueArray
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)

newtype TimeSpace
  = TimeSpace
  { title :: String
  , description :: String
  , timeScale :: TimeScale
  , siblings :: UniqueArray (EventOrTimeSpanPoly EventID TimeSpanID) -- TODO manual field sorting
  , timelines :: UniqueArray TimelineID
  , id :: TimeSpaceID
  , parent :: Maybe RootTimeSpaceOrParentID
  }

derive instance genericTimeSpace :: Generic TimeSpace _

derive newtype instance eqTimeSpace :: Eq TimeSpace

derive newtype instance showTimeSpace :: Show TimeSpace

instance encodeJsonTimeSpace :: EncodeJson TimeSpace where
  encodeJson (TimeSpace { title, description, timeScale, siblings, timelines, id, parent }) =
    "title" := title
      ~> "description"
      := description
      ~> "timeScale"
      := timeScale
      ~> "siblings"
      := siblings
      ~> "timelines"
      := timelines
      ~> "id"
      := id
      ~> "parent"
      := parent
      ~> jsonEmptyObject

instance decodeJsonTimeSpace :: DecodeJson TimeSpace where
  decodeJson json = do
    o <- decodeJson json
    title <- o .: "title"
    description <- o .: "description"
    timeScale <- o .: "timeScale"
    siblings <- o .: "siblings"
    timelines <- o .: "timelines"
    id <- o .: "id"
    parent <- o .: "parent"
    pure (TimeSpace { title, description, timeScale, siblings, timelines, id, parent })

instance arbitraryEvent :: Arbitrary TimeSpace where
  arbitrary = do
    title <- genString
    description <- genString
    timeScale <- arbitrary
    siblings <- arbitrary
    timelines <- arbitrary
    id <- arbitrary
    parent <- arbitrary
    pure (TimeSpace { title, description, timeScale, siblings, timelines, id, parent })

defaultTimeSpace :: Effect TimeSpace
defaultTimeSpace = newTimeSpace { title: "TimeSpace Name", description: "", timeScale: def, parent: Nothing }

newTimeSpace ::
  { title :: String
  , description :: String
  , timeScale :: TimeScale
  , parent :: Maybe RootTimeSpaceOrParentID
  } ->
  Effect TimeSpace
newTimeSpace { title, description, timeScale, parent } = do
  id <- UUID.genUUID
  pure
    $ TimeSpace
        { title
        , description
        , timeScale
        , siblings: UniqueArray.empty
        , timelines: UniqueArray.empty
        , id: TimeSpaceID id
        , parent
        }
