-- | Repo table rendering — table, rows, and badges.
module View.RepoTable
  ( renderRepoTable
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Repo(..))
import View.Detail (renderDetailPanel)
import View.Helpers (formatDate, linkButton)
import View.Types (Action(..), State)

-- | The repo table with rows.
renderRepoTable
  :: forall w
   . State
  -> Array Repo
  -> HH.HTML w Action
renderRepoTable state repos =
  HH.table
    [ HP.class_ (HH.ClassName "repo-table") ]
    [ HH.thead_
        [ HH.tr_
            [ HH.th_ []
            , HH.th_
                [ HH.text "Repository" ]
            , HH.th_
                [ HH.text "Description" ]
            , HH.th_ [ HH.text "Lang" ]
            , HH.th_ [ HH.text "Vis" ]
            , HH.th_ [ HH.text "Issues" ]
            , HH.th_
                [ HH.text "Updated" ]
            , HH.th_ []
            , HH.th_ []
            ]
        ]
    , HH.tbody_
        (repos >>= \r -> renderRepoRow state r)
    ]

-- | A single repo row, plus detail panel if expanded.
renderRepoRow
  :: forall w
   . State
  -> Repo
  -> Array (HH.HTML w Action)
renderRepoRow state (Repo r) =
  let
    isExpanded = state.expanded == Just r.fullName
    rowClass =
      if isExpanded then "repo-row expanded"
      else "repo-row"
  in
    [ HH.tr
        [ HE.onClick \_ -> ToggleExpand r.fullName
        , HP.class_ (HH.ClassName rowClass)
        , HE.onDrop \_ -> DragDrop r.fullName
        , HP.attr (AttrName "ondragover")
            "event.preventDefault()"
        ]
        [ HH.td
            [ HP.class_
                (HH.ClassName "drag-handle")
            , HP.draggable true
            , HE.onDragStart \_ -> DragStart
                r.fullName
            ]
            [ HH.text "\x2630" ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    (HH.ClassName "repo-name")
                ]
                [ HH.text r.name ]
            ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    (HH.ClassName "repo-desc")
                ]
                [ HH.text
                    (fromMaybe "" r.description)
                ]
            ]
        , HH.td_ [ renderLangBadge r.language ]
        , HH.td_
            [ renderVisBadge r.visibility ]
        , HH.td_
            [ renderCountBadge "badge-issues"
                r.openIssuesCount
            ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    (HH.ClassName "repo-date")
                ]
                [ HH.text
                    (formatDate r.updatedAt)
                ]
            ]
        , HH.td_
            [ linkButton r.htmlUrl ]
        , HH.td_
            [ HH.button
                [ HE.onClick \_ -> RemoveRepo
                    r.fullName
                , HP.class_
                    (HH.ClassName "btn-remove")
                ]
                [ HH.text "\x2715" ]
            ]
        ]
    ]
      <>
        if isExpanded then
          [ renderDetailPanel state ]
        else []

-- | Language badge.
renderLangBadge
  :: forall w i. Maybe String -> HH.HTML w i
renderLangBadge = case _ of
  Nothing -> HH.text ""
  Just lang ->
    HH.span
      [ HP.class_ (HH.ClassName "badge badge-lang") ]
      [ HH.text lang ]

-- | Visibility badge.
renderVisBadge
  :: forall w i. String -> HH.HTML w i
renderVisBadge vis =
  HH.span
    [ HP.class_
        ( HH.ClassName
            ( "badge "
                <>
                  if vis == "private" then
                    "badge-private"
                  else "badge-public"
            )
        )
    ]
    [ HH.text vis ]

-- | Count badge (issues/PRs).
renderCountBadge
  :: forall w i. String -> Int -> HH.HTML w i
renderCountBadge cls n =
  HH.span
    [ HP.class_
        ( HH.ClassName
            ( "badge " <> cls
                <>
                  if n == 0 then " badge-zero"
                  else ""
            )
        )
    ]
    [ HH.text (show n) ]
