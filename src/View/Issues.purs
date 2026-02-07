-- | Issues section rendering.
module View.Issues
  ( renderIssuesSection
  ) where

import Prelude

import Data.Array (any, filter, length, null, partition)
import Data.Set as Set
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Issue(..))
import View.DetailWidgets
  ( collectLabels
  , copyButton
  , hideButton
  , refreshButton
  , renderLabelSelector
  )
import View.Helpers
  ( detailHead
  , formatDate
  , linkButton
  , renderAssignees
  , renderAuthor
  , renderLabels
  , renderMarkdownRow
  )
import View.Types (Action(..), State)

-- | Issues sub-section with visible/hidden partitions.
renderIssuesSection
  :: forall w
   . State
  -> Array Issue
  -> Int
  -> HH.HTML w Action
renderIssuesSection state issues count =
  let
    key = "section-issues"
    hiddenKey = "hidden-issues"
    isOpen = Set.member key state.expandedItems
    hiddenOpen = Set.member hiddenKey
      state.expandedItems
    allLabels = collectLabels
      (map (\(Issue i) -> i.labels) issues)
    filtered =
      if Set.isEmpty state.issueLabelFilters then
        issues
      else filter
        ( \(Issue i) ->
            any
              ( \l ->
                  Set.member l.name
                    state.issueLabelFilters
              )
              i.labels
        )
        issues
    { yes: hidden, no: visible } = partition
      ( \(Issue i) ->
          Set.member i.htmlUrl state.hiddenItems
      )
      filtered
    hiddenCount = length hidden
  in
    HH.div
      [ HP.class_ (HH.ClassName "detail-section") ]
      [ HH.div
          [ HP.class_
              ( HH.ClassName
                  "detail-heading clickable"
              )
          , HE.onClick \_ -> ToggleItem key
          ]
          [ HH.text
              ( ( if isOpen then "\x25BE "
                  else "\x25B8 "
                )
                  <> "Issues ("
                  <> show count
                  <> ")"
              )
          , if state.issuesLoading then
              HH.span
                [ HP.class_
                    (HH.ClassName "loading")
                ]
                [ HH.text " \x21BB" ]
            else if not isOpen && count == 0 then
              HH.text ""
            else refreshButton RefreshIssues
          ]
      , if isOpen && not (null allLabels) then
          renderLabelSelector
            state.issueLabelFilters
            ToggleIssueLabelFilter
            allLabels
        else HH.text ""
      , if not isOpen then HH.text ""
        else if null issues then
          HH.div
            [ HP.class_ (HH.ClassName "empty-msg") ]
            [ HH.text "No open issues" ]
        else
          HH.table
            [ HP.class_
                (HH.ClassName "detail-table")
            ]
            ( [ detailHead
              , HH.tbody_
                  ( if null visible then
                      [ HH.tr_
                          [ HH.td
                              [ HP.colSpan 6 ]
                              [ HH.span
                                  [ HP.class_
                                      ( HH.ClassName
                                          "empty-msg"
                                      )
                                  ]
                                  [ HH.text
                                      "All issues hidden"
                                  ]
                              ]
                          ]
                      ]
                    else
                      visible >>= renderIssueRow
                        state
                        false
                  )
              ]
                <>
                  if null hidden then []
                  else
                    [ HH.tbody_
                        [ HH.tr_
                            [ HH.td
                                [ HP.colSpan 6
                                , HP.class_
                                    ( HH.ClassName
                                        "hidden-separator clickable"
                                    )
                                , HE.onClick
                                    \_ ->
                                      ToggleItem
                                        hiddenKey
                                ]
                                [ HH.text
                                    ( ( if hiddenOpen then
                                          "\x25BE "
                                        else
                                          "\x25B8 "
                                      )
                                        <> "Hidden ("
                                        <> show
                                          hiddenCount
                                        <> ")"
                                    )
                                ]
                            ]
                        ]
                    , if not hiddenOpen then
                        HH.text ""
                      else
                        HH.tbody_
                          ( hidden
                              >>= renderIssueRow
                                state
                                true
                          )
                    ]
            )
      ]

-- | Single issue row + optional body row.
renderIssueRow
  :: forall w
   . State
  -> Boolean
  -> Issue
  -> Array (HH.HTML w Action)
renderIssueRow state isHidden (Issue i) =
  let
    prefix = if isHidden then "h-issue-" else "issue-"
    key = prefix <> show i.number
    isOpen = Set.member key state.expandedItems
  in
    [ HH.tr
        [ HE.onClick \_ -> ToggleItem key
        , HP.class_ (HH.ClassName "repo-row")
        ]
        [ HH.td_
            [ refreshButton
                (RefreshIssue i.number)
            , hideButton i.htmlUrl isHidden
            , linkButton i.htmlUrl
            , copyButton i.title
            ]
        , HH.td_
            [ HH.span_
                [ HH.text
                    ( "#" <> show i.number
                        <> " "
                        <> i.title
                    )
                ]
            , renderLabels i.labels
            ]
        , HH.td_
            [ renderAssignees i.assignees ]
        , HH.td_
            [ renderAuthor i.userLogin ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    (HH.ClassName "detail-date")
                ]
                [ HH.text
                    (formatDate i.createdAt)
                ]
            ]
        ]
    ]
      <>
        if isOpen then
          renderMarkdownRow i.body
        else []
