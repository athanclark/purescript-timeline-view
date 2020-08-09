module Timeline.UI.TimeSpan where

import Timeline.ID.TimeSpace (TimeSpaceID (..))
import Timeline.ID.TimeSpan (TimeSpanID (..))
import Timeline.ID.Timeline (TimelineID (..))
import Timeline.ID.ChildOrSiblingParent (ChildOrSiblingParentID (..))
import Timeline.Time.Span (DecidedSpan(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either (..))
import Data.UUID (genUUID) as UUID
import Data.Generic.Rep (class Generic)
import Data.Default (class Default)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , decodeJson
  , (:=)
  , (~>)
  , jsonEmptyObject
  , (.:)
  )
import Effect.Unsafe (unsafePerformEffect)
import Effect.Random (randomBool)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)

-- | An inclusive span of time from `startIndex` to `stopIndex`.
-- |
-- | Defined over the user-level timescale `a`.
newtype TimeSpan
  = TimeSpan
  { name :: String
  , description :: String
  , span :: DecidedSpan
  , timeSpace :: Maybe TimeSpaceID
  , id :: TimeSpanID -- TODO trim the fat later
  , parent :: Maybe ChildOrSiblingParentID
  }

derive instance genericTimeSpan :: Generic TimeSpan _

derive newtype instance eqTimeSpan :: Eq TimeSpan

derive newtype instance showTimeSpan :: Show TimeSpan

instance encodeJsonTimeSpan :: EncodeJson TimeSpan where
  encodeJson (TimeSpan { name, description, span, timeSpace, id, parent }) =
    "name" := name
      ~> "description"
      := description
      ~> "span"
      := span
      ~> "timeSpace"
      := timeSpace
      ~> "id"
      := id
      ~> "parent"
      := parent
      ~> jsonEmptyObject

instance decodeJsonTimeSpan :: DecodeJson TimeSpan where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    span <- o .: "span"
    timeSpace <- o .: "timeSpace"
    id <- o .: "id"
    parent <- o .: "parent"
    pure $ TimeSpan { name, description, span, timeSpace, id, parent }

instance arbitraryTimeSpan :: Arbitrary TimeSpan where
  arbitrary = do
    name <- genString
    description <- genString
    span <- arbitrary
    timeSpace <- arbitrary
    id <- arbitrary
    parent <- arbitrary
    pure (TimeSpan { name, description, span, timeSpace, id, parent })

instance defaultTimeSpan :: Default TimeSpan where
  def =
    TimeSpan
      { name: "TimeSpan"
      , description: ""
      , span: DecidedSpanNumber { start: 0.0, stop: 1.0 }
      , timeSpace: Nothing
      , id: TimeSpanID (unsafePerformEffect UUID.genUUID)
      , parent: unsafePerformEffect do
        let genCorS = do
              cOrS <- randomBool
              ChildOrSiblingParentID <$>
                if cOrS
                  then Left <<< TimelineID <$> UUID.genUUID
                  else Right <<< TimeSpaceID <$> UUID.genUUID
        exists <- randomBool
        if exists then Just <$> genCorS else pure Nothing
      }
