-- | Shared widgets for detail panel rows.
module View.DetailWidgets
  ( refreshButton
  , copyButton
  , hideButton
  , collectLabels
  , renderLabelSelector
  ) where

import Prelude

import Data.Array (concatMap, sort)
import Data.Set as Set
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import View.Types (Action(..))

-- | Refresh button for a single item.
refreshButton
  :: forall w. Action -> HH.HTML w Action
refreshButton action =
  HH.button
    [ HE.onClick \_ -> action
    , HP.class_ (HH.ClassName "btn-hide")
    , HP.title "Refresh"
    , HP.attr (AttrName "onclick")
        "event.stopPropagation()"
    ]
    [ HH.text "\x21BB" ]

-- | Copy title to clipboard button.
copyButton
  :: forall w. String -> HH.HTML w Action
copyButton text =
  HH.button
    [ HE.onClick \_ -> CopyText text
    , HP.class_ (HH.ClassName "btn-hide")
    , HP.title "Copy title"
    , HP.attr (AttrName "onclick")
        "event.stopPropagation()"
    ]
    [ HH.text "\x2398" ]

-- | Hide/unhide toggle button.
hideButton
  :: forall w. String -> Boolean -> HH.HTML w Action
hideButton url isHidden =
  HH.button
    [ HE.onClick \_ -> HideItem url
    , HP.class_ (HH.ClassName "btn-hide")
    , HP.title
        (if isHidden then "Unhide" else "Hide")
    ]
    [ HH.text
        (if isHidden then "\x25C9" else "\x25CC")
    ]

-- | Collect unique sorted label names from items.
collectLabels
  :: Array (Array { name :: String }) -> Array String
collectLabels =
  sort <<< Set.toUnfoldable <<< Set.fromFoldable
    <<< map _.name
    <<< concatMap identity

-- | Vertical label selector with multi-select.
renderLabelSelector
  :: forall w
   . Set.Set String
  -> (String -> Action)
  -> Array String
  -> HH.HTML w Action
renderLabelSelector active toAction labels =
  HH.div
    [ HP.class_
        (HH.ClassName "label-selector")
    ]
    ( map
        ( \name ->
            HH.span
              [ HP.class_
                  ( HH.ClassName
                      ( "label-tag clickable"
                          <>
                            if Set.member name active then " active"
                            else ""
                      )
                  )
              , HE.onClick \_ -> toAction name
              ]
              [ HH.text name ]
        )
        labels
    )
