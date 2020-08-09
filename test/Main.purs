module Test.Main where

import Timeline.UI.Event (Event) as UI
import Timeline.UI.TimeSpan (TimeSpan) as UI
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpan) as UI
import Timeline.UI.Settings (Settings) as UI
import Timeline.UI.Timeline (Timeline) as UI
import Timeline.UI.TimeSpace (TimeSpace) as UI
import Timeline.UI.TimeSpace.TimeScale (TimeScale) as UI

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Identity (Identity)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Test.QuickCheck (class Arbitrary, quickCheck, Result (..))
import Test.Spec (describe, it, SpecT)
import Test.Spec.Runner (runSpec', defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Type.Proxy (Proxy (..))


main :: Effect Unit
main = launchAff_ $ runSpec' (defaultConfig {timeout = Nothing}) [consoleReporter] tests

tests :: SpecT Aff Unit Identity Unit
tests = do
  describe "Json" do
    jsonTest "TimeSpace" (Proxy :: Proxy UI.TimeSpace)
    jsonTest "TimeScale" (Proxy :: Proxy UI.TimeScale)
    jsonTest "Timeline" (Proxy :: Proxy UI.Timeline)
    jsonTest "Event" (Proxy :: Proxy UI.Event)
    jsonTest "TimeSpan" (Proxy :: Proxy UI.TimeSpan)
    jsonTest "EventOrTimeSpan" (Proxy :: Proxy UI.EventOrTimeSpan)
    jsonTest "Settings" (Proxy :: Proxy UI.Settings)
  where
    jsonTest :: forall a
              . Arbitrary a
             => Show a
             => Eq a
             => EncodeJson a
             => DecodeJson a
             => String -> Proxy a -> _
    jsonTest name proxy = it name (liftEffect (quickCheck (jsonIso proxy)))


jsonIso :: forall a
         . Eq a
        => Show a
        => EncodeJson a
        => DecodeJson a
        => Proxy a -> a -> Result
jsonIso Proxy x =
  -- trace x \_ ->
  let result = decodeJson (encodeJson x)
  in  case result of
        Left e -> Failed $ "Couldn't parse: " <> show e
        Right y
          | y == x -> Success
          | otherwise ->
              Failed $ "Not equal - original " <> show (show x) <> "\nresult: " <> show (show y)
