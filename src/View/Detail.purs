-- | Detail panel rendering — issues, PRs, and check runs.
module View.Detail
  ( renderDetailPanel
  ) where

import Prelude

import Data.Array (all, any, filter, length, null, partition)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (CheckRun(..), Issue(..), PullRequest(..))
import View.Helpers
  ( detailHead
  , formatDate
  , linkButton
  , renderAssignees
  , renderLabels
  , renderMarkdownRow
  )
import View.Types (Action(..), State)

-- | Detail panel shown below expanded row.
renderDetailPanel
  :: forall w. State -> HH.HTML w Action
renderDetailPanel state =
  HH.tr
    [ HP.class_ (HH.ClassName "detail-panel") ]
    [ HH.td
        [ HP.colSpan 9 ]
        [ if state.detailLoading then
            HH.div
              [ HP.class_
                  (HH.ClassName "loading-spinner")
              ]
              [ HH.text "Loading issues and PRs..." ]
          else case state.details of
            Nothing ->
              HH.div_
                [ renderIssuesSection state [] 0
                , renderPRsSection state [] 0
                ]
            Just detail ->
              HH.div_
                [ renderIssuesSection state
                    detail.issues
                    detail.issueCount
                , renderPRsSection state
                    detail.pullRequests
                    detail.prCount
                ]
        ]
    ]

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
    { yes: hidden, no: visible } = partition
      ( \(Issue i) ->
          Set.member i.htmlUrl state.hiddenItems
      )
      issues
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
          , refreshButton RefreshIssues
          ]
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
            [ HH.span
                [ HP.class_
                    (HH.ClassName "detail-link")
                ]
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
            [ HH.span
                [ HP.class_
                    (HH.ClassName "detail-author")
                ]
                [ HH.text i.userLogin ]
            ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    (HH.ClassName "detail-date")
                ]
                [ HH.text
                    (formatDate i.createdAt)
                ]
            ]
        , HH.td_
            [ linkButton i.htmlUrl ]
        , HH.td_
            [ refreshButton
                (RefreshIssue i.number)
            , hideButton i.htmlUrl isHidden
            ]
        ]
    ]
      <>
        if isOpen then
          renderMarkdownRow i.body
        else []

-- | PRs sub-section with visible/hidden partitions.
renderPRsSection
  :: forall w
   . State
  -> Array PullRequest
  -> Int
  -> HH.HTML w Action
renderPRsSection state prs count =
  let
    key = "section-prs"
    hiddenKey = "hidden-prs"
    isOpen = Set.member key state.expandedItems
    hiddenOpen = Set.member hiddenKey
      state.expandedItems
    { yes: hidden, no: visible } = partition
      ( \(PullRequest p) ->
          Set.member p.htmlUrl state.hiddenItems
      )
      prs
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
                  <> "Pull Requests ("
                  <> show count
                  <> ")"
              )
          , refreshButton RefreshPRs
          ]
      , if not isOpen then HH.text ""
        else if null prs then
          HH.div
            [ HP.class_ (HH.ClassName "empty-msg") ]
            [ HH.text "No open pull requests" ]
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
                                      "All pull requests hidden"
                                  ]
                              ]
                          ]
                      ]
                    else
                      visible >>= renderPRRow
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
                          ( hidden >>= renderPRRow
                              state
                              true
                          )
                    ]
            )
      ]

-- | Single PR row + optional body/checks rows.
renderPRRow
  :: forall w
   . State
  -> Boolean
  -> PullRequest
  -> Array (HH.HTML w Action)
renderPRRow state isHidden (PullRequest pr) =
  let
    prefix = if isHidden then "h-pr-" else "pr-"
    key = prefix <> show pr.number
    isOpen = Set.member key state.expandedItems
    checks = state.details >>= \d ->
      Map.lookup pr.number d.prChecks
    combined = combineCheckRuns
      (fromMaybe [] checks)
  in
    [ HH.tr
        [ HE.onClick \_ -> ToggleItem key
        , HP.class_ (HH.ClassName "repo-row")
        ]
        [ HH.td_
            ( [ HH.span
                  [ HP.class_
                      (HH.ClassName "detail-link")
                  ]
                  [ HH.text
                      ( "#" <> show pr.number
                          <> " "
                          <> pr.title
                      )
                  ]
              , renderLabels pr.labels
              ]
                <>
                  ( if pr.draft then
                      [ HH.span
                          [ HP.class_
                              ( HH.ClassName
                                  "label-tag draft-tag"
                              )
                          ]
                          [ HH.text "draft" ]
                      ]
                    else []
                  )
                <> [ statusBadge checks combined ]
            )
        , HH.td_
            [ renderAssignees pr.assignees ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    (HH.ClassName "detail-author")
                ]
                [ HH.text pr.userLogin ]
            ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    (HH.ClassName "detail-date")
                ]
                [ HH.text
                    (formatDate pr.createdAt)
                ]
            ]
        , HH.td_
            [ linkButton pr.htmlUrl ]
        , HH.td_
            [ refreshButton
                (RefreshPR pr.number)
            , hideButton pr.htmlUrl isHidden
            ]
        ]
    ]
      <>
        if isOpen then
          renderMarkdownRow pr.body
            <> renderCheckRuns state pr.number
              checks
        else []

-- | Refresh button for a single item.
refreshButton
  :: forall w. Action -> HH.HTML w Action
refreshButton action =
  HH.button
    [ HE.onClick \_ -> action
    , HP.class_ (HH.ClassName "btn-hide")
    , HP.attr (AttrName "onclick")
        "event.stopPropagation()"
    ]
    [ HH.text "\x21BB" ]

-- | Hide/unhide toggle button.
hideButton
  :: forall w. String -> Boolean -> HH.HTML w Action
hideButton url isHidden =
  HH.button
    [ HE.onClick \_ -> HideItem url
    , HP.class_ (HH.ClassName "btn-hide")
    ]
    [ HH.text
        (if isHidden then "\x25C9" else "\x25CC")
    ]

-- | Derive combined state from check runs.
combineCheckRuns :: Array CheckRun -> String
combineCheckRuns runs
  | null runs = "unknown"
  | any (\(CheckRun r) -> r.status /= "completed")
      runs = "pending"
  | any
      (\(CheckRun r) -> r.conclusion == Just "failure")
      runs = "failure"
  | any
      ( \(CheckRun r) ->
          r.conclusion == Just "cancelled"
      )
      runs = "cancelled"
  | all
      (\(CheckRun r) -> r.conclusion == Just "success")
      runs = "success"
  | otherwise = "mixed"

-- | CI status badge for a PR (derived from checks).
statusBadge
  :: forall w i
   . Maybe (Array CheckRun)
  -> String
  -> HH.HTML w i
statusBadge Nothing _ = HH.text ""
statusBadge (Just _) combined =
  HH.span
    [ HP.class_
        ( HH.ClassName
            ( "label-tag ci-badge ci-"
                <> combined
            )
        )
    ]
    [ HH.text combined ]

-- | Render non-success check runs as collapsible section.
renderCheckRuns
  :: forall w
   . State
  -> Int
  -> Maybe (Array CheckRun)
  -> Array (HH.HTML w Action)
renderCheckRuns _ _ Nothing = []
renderCheckRuns state prNum (Just runs) =
  let
    failed = filter
      ( \(CheckRun r) ->
          r.conclusion /= Just "success"
      )
      runs
    key = "checks-" <> show prNum
    isOpen = Set.member key state.expandedItems
  in
    if null failed then []
    else
      [ HH.tr
          [ HP.class_ (HH.ClassName "detail-row") ]
          [ HH.td
              [ HP.colSpan 6
              , HP.class_
                  ( HH.ClassName
                      "hidden-separator clickable"
                  )
              , HE.onClick \_ -> ToggleItem key
              ]
              [ HH.text
                  ( ( if isOpen then "\x25BE "
                      else "\x25B8 "
                    )
                      <> "Checks ("
                      <> show (length failed)
                      <> ")"
                  )
              ]
          ]
      ]
        <>
          if isOpen then
            [ HH.tr
                [ HP.class_
                    (HH.ClassName "detail-row")
                ]
                [ HH.td
                    [ HP.colSpan 6 ]
                    [ HH.div
                        [ HP.class_
                            (HH.ClassName "check-runs")
                        ]
                        (map renderCheckRun failed)
                    ]
                ]
            ]
          else []

-- | Render a single check run.
renderCheckRun :: forall w i. CheckRun -> HH.HTML w i
renderCheckRun (CheckRun run) =
  let
    st = fromMaybe run.status run.conclusion
  in
    HH.div
      [ HP.class_
          (HH.ClassName "check-run")
      ]
      [ HH.span
          [ HP.class_
              ( HH.ClassName
                  ("label-tag ci-badge ci-" <> st)
              )
          ]
          [ HH.text st ]
      , HH.a
          [ HP.href run.htmlUrl
          , HP.target "_blank"
          , HP.class_
              (HH.ClassName "detail-link check-name")
          ]
          [ HH.text run.name ]
      ]
