-- | Pull requests section rendering.
module View.PRs
  ( renderPRsSection
  ) where

import Prelude

import Data.Array
  ( all
  , any
  , filter
  , length
  , null
  , partition
  , sort
  )
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (CheckRun(..), PullRequest(..))
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
    prStatus pr =
      state.details >>= \d ->
        Map.lookup pr d.prChecks
          <#> combineCheckRuns
    allCiNames = filter (_ /= "unknown")
      $ map
          ( \(PullRequest p) ->
              fromMaybe "unknown" (prStatus p.number)
          )
          prs
    ciUnique = sort $ Set.toUnfoldable
      $ Set.fromFoldable allCiNames
    ciStatuses = map
      ( \n ->
          { name: n
          , count: length
              (filter (_ == n) allCiNames)
          }
      )
      ciUnique
    allLabels =
      collectLabels
        (map (\(PullRequest p) -> p.labels) prs)
        <> ciStatuses
    filtered =
      if Set.isEmpty state.prLabelFilters then prs
      else filter
        ( \(PullRequest p) ->
            any
              ( \l ->
                  Set.member l.name
                    state.prLabelFilters
              )
              p.labels
              || case prStatus p.number of
                Just st ->
                  Set.member st
                    state.prLabelFilters
                Nothing -> false
        )
        prs
    { yes: hidden, no: visible } = partition
      ( \(PullRequest p) ->
          Set.member p.htmlUrl state.hiddenItems
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
                  <> "Pull Requests ("
                  <> show count
                  <> ")"
              )
          , if state.prsLoading then
              HH.span
                [ HP.class_
                    (HH.ClassName "loading")
                ]
                [ HH.text " \x21BB" ]
            else if not isOpen && count == 0 then
              HH.text ""
            else refreshButton RefreshPRs
          ]
      , if isOpen && not (null allLabels) then
          renderLabelSelector
            state.prLabelFilters
            TogglePRLabelFilter
            allLabels
        else HH.text ""
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
            [ refreshButton
                (RefreshPR pr.number)
            , hideButton pr.htmlUrl isHidden
            , linkButton pr.htmlUrl
            , copyButton pr.title
            ]
        , HH.td_
            ( [ HH.span_
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
            [ renderAuthor pr.userLogin ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    (HH.ClassName "detail-date")
                ]
                [ HH.text
                    (formatDate pr.createdAt)
                ]
            ]
        ]
    ]
      <>
        if isOpen then
          renderMarkdownRow pr.body
            <> renderCheckRuns state pr.number
              checks
        else []

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
