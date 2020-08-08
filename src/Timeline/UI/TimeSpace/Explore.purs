module Timeline.UI.TimeSpace.Explore where

import Timeline.ID.TimeSpace (TimeSpaceID(..))
import Timeline.Time.MaybeLimit (DecidedMaybeLimit(..), MaybeLimit(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.UUID (genUUID) as UUID
import Data.Array (uncons, unsnoc, snoc, foldl, foldr) as Array
import Data.Default (class Default)
import Data.Array.Indexed (IxArray)
import Data.Array.Indexed (intoFrom, update', lookup, fromFoldable) as IxArray
import Data.Array.Unique (UniqueArray)
import Data.Array.Unique (unsafeMap, length, unsafeFromArray, empty) as UniqueArray
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)

-- | A rose tree of simplified recursive timespaces.
newtype ExploreTimeSpaces
  = ExploreTimeSpaces
  { title :: String
  , limit :: DecidedMaybeLimit
  , id :: TimeSpaceID
  , children :: UniqueArray ExploreTimeSpaces
  }

instance defaultExploreTimeSpaces :: Default ExploreTimeSpaces where
  def =
    ExploreTimeSpaces
      { title: "TimeSpace Name"
      , limit:
          DecidedMaybeLimitNumber
            $ JustLimitBounds
                { begin: 1234.0
                , end: 5678.0
                }
      , id: TimeSpaceID (unsafePerformEffect UUID.genUUID)
      , children:
          UniqueArray.unsafeFromArray
            [ ExploreTimeSpaces
                { title: "TimeSpace Child 1"
                , limit: DecidedMaybeLimitNumber $ JustLimitBounds { begin: 0.0, end: 1.0 }
                , id: TimeSpaceID (unsafePerformEffect UUID.genUUID)
                , children: UniqueArray.empty
                }
            , ExploreTimeSpaces
                { title: "TimeSpace Child 2"
                , limit: DecidedMaybeLimitNumber $ JustLimitBounds { begin: 1.0, end: 2.0 }
                , id: TimeSpaceID (unsafePerformEffect UUID.genUUID)
                , children:
                    UniqueArray.unsafeFromArray
                      [ ExploreTimeSpaces
                          { title: "TimeSpace GrandChild 1"
                          , limit: DecidedMaybeLimitNumber $ JustLimitBounds { begin: 2.0, end: 3.0 }
                          , id: TimeSpaceID (unsafePerformEffect UUID.genUUID)
                          , children: UniqueArray.empty
                          }
                      , ExploreTimeSpaces
                          { title: "TimeSpace GrandChild 2"
                          , limit: DecidedMaybeLimitNumber $ JustLimitBounds { begin: 4.0, end: 5.0 }
                          , id: TimeSpaceID (unsafePerformEffect UUID.genUUID)
                          , children: UniqueArray.empty
                          }
                      ]
                }
            , ExploreTimeSpaces
                { title: "TimeSpace Child 3"
                , limit: DecidedMaybeLimitNumber $ JustLimitBounds { begin: 6.0, end: 7.0 }
                , id: TimeSpaceID (unsafePerformEffect UUID.genUUID)
                , children: UniqueArray.empty
                }
            ]
      }

-- | Should be treated as only used by the dialog in it's component state.
-- |
-- | `aux` represents arbitrary auxiliary information required by the component
-- | state; for instance, an "open" boolean, determining if the dropdown is
-- | open.
newtype ExploreTimeSpacesWithAux aux
  = ExploreTimeSpacesWithAux
  { title :: String
  , limit :: DecidedMaybeLimit
  , id :: TimeSpaceID
  , children ::
      Maybe
        { aux :: aux
        , childrenValues :: IxArray (ExploreTimeSpacesWithAux aux)
        }
  }

-- | Initial state for explore time spaces
exploreTimeSpacesWithAux :: forall aux. aux -> ExploreTimeSpaces -> ExploreTimeSpacesWithAux aux
exploreTimeSpacesWithAux defAux (ExploreTimeSpaces { title, limit, id, children }) =
  ExploreTimeSpacesWithAux
    { title
    , limit
    , id
    , children:
        if UniqueArray.length children == 0 then
          Nothing
        else
          Just
            { aux: defAux
            , childrenValues:
                let
                  makeKeyValue x@(ExploreTimeSpaces { id: id' }) = Tuple (show id') (exploreTimeSpacesWithAux defAux x)
                in
                  IxArray.fromFoldable (UniqueArray.unsafeMap makeKeyValue children)
            }
    }

-- | Uses the signal of ExploreTimeSpaces to update an existing value, deleting or appending where necessary.
updateExploreTimeSpacesWithAux :: forall aux. aux -> ExploreTimeSpacesWithAux aux -> ExploreTimeSpaces -> ExploreTimeSpacesWithAux aux
updateExploreTimeSpacesWithAux defAux (ExploreTimeSpacesWithAux x) (ExploreTimeSpaces y) =
  ExploreTimeSpacesWithAux
    x
      -- id's should be the same - no need to overwrite...?
      { title = y.title
      , limit = y.limit
      , children =
        if UniqueArray.length y.children == 0 then
          Nothing
        else
          Just
            $ let
                makeKeyValue :: forall a. (ExploreTimeSpaces -> a) -> ExploreTimeSpaces -> Tuple String a
                makeKeyValue finishMutation timeSpace@(ExploreTimeSpaces { id }) = Tuple (show id) (finishMutation timeSpace)

                yChildrenValues :: forall a. (ExploreTimeSpaces -> a) -> IxArray a
                yChildrenValues finishMutation = IxArray.fromFoldable (UniqueArray.unsafeMap (makeKeyValue finishMutation) y.children)
              in
                case x.children of
                  Nothing ->
                    { aux: defAux
                    , childrenValues: yChildrenValues (exploreTimeSpacesWithAux defAux)
                    }
                  Just { aux, childrenValues } ->
                    { aux
                    , childrenValues:
                        IxArray.intoFrom (updateExploreTimeSpacesWithAux defAux) (exploreTimeSpacesWithAux defAux) childrenValues (yChildrenValues identity)
                    }
      }

-- | Updates auxillary value at a specific rose-tree index. Fails silently if nonexistent.
updateAux :: forall aux. (aux -> aux) -> Array TimeSpaceID -> ExploreTimeSpacesWithAux aux -> ExploreTimeSpacesWithAux aux
updateAux f indicies orig@(ExploreTimeSpacesWithAux x) = case x.children of
  Nothing -> orig
  Just { aux, childrenValues } -> case Array.uncons indicies of
    Nothing -> ExploreTimeSpacesWithAux x { children = Just { aux: f aux, childrenValues } }
    Just { head: index, tail: restOfIndicies } -> case IxArray.lookup (show index) childrenValues of
      Nothing -> orig
      Just foundChild ->
        ExploreTimeSpacesWithAux
          x
            { children =
              Just
                { childrenValues: IxArray.update' (show index) (updateAux f restOfIndicies foundChild) childrenValues
                , aux
                }
            }

-- | Assigns an auxillary value
setAux :: forall aux. aux -> Array TimeSpaceID -> ExploreTimeSpacesWithAux aux -> ExploreTimeSpacesWithAux aux
setAux x = updateAux (const x)

-- | Assigns an auxillary value for all values preceeding the target index
setAuxPreceeding :: forall aux. aux -> Array TimeSpaceID -> ExploreTimeSpacesWithAux aux -> ExploreTimeSpacesWithAux aux
setAuxPreceeding auxVal indicies tree =
  let
    -- All indicies up to the target
    allPreceedingIndicies :: Array (Array TimeSpaceID)
    allPreceedingIndicies =
      let
        -- Starts with the "root" index
        basis = [ [] ]

        go :: Array (Array TimeSpaceID) -> TimeSpaceID -> _
        go acc index =
          unsafePartial
            $ case Array.unsnoc acc of
                Just { last } -> Array.snoc acc (Array.snoc last index)
      in
        Array.foldl go basis indicies
  in
    Array.foldr (setAux auxVal) tree allPreceedingIndicies
