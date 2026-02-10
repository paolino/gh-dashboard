-- | Workflows section rendering.
module View.Workflows
  ( renderWorkflowsSection
  ) where

import Prelude

import Data.Array
  ( filter
  , index
  , length
  , nubByEq
  , null
  , sort
  , sortBy
  )
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (take) as String
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (CommitPR, WorkflowJob(..), WorkflowRun(..))
import View.DetailWidgets
  ( refreshButton
  , renderLabelSelector
  )
import View.Helpers (formatDateTime, linkButton)
import View.Types (Action(..), State)

-- | Derive effective status for a workflow run.
runStatus :: WorkflowRun -> String
runStatus (WorkflowRun r) =
  fromMaybe r.status r.conclusion

-- | Extract unique SHAs from runs, preserving order.
extractShas :: Array WorkflowRun -> Array String
extractShas runs =
  map (\(WorkflowRun r) -> r.headSha)
    ( nubByEq
        ( \(WorkflowRun a) (WorkflowRun b) ->
            a.headSha == b.headSha
        )
        runs
    )

-- | Runs for a given SHA, deduplicated by name, recent first.
runsForSha
  :: String -> Array WorkflowRun -> Array WorkflowRun
runsForSha sha =
  sortBy
    ( \(WorkflowRun a) (WorkflowRun b) ->
        compare b.updatedAt a.updatedAt
    )
    <<< nubByEq
      ( \(WorkflowRun a) (WorkflowRun b) ->
          a.name == b.name
      )
    <<< filter
      (\(WorkflowRun r) -> r.headSha == sha)

-- | Workflows sub-section with collapsible toggle.
renderWorkflowsSection
  :: forall w
   . State
  -> Array WorkflowRun
  -> Int
  -> HH.HTML w Action
renderWorkflowsSection state allRuns shaCount =
  let
    key = "section-workflows"
    isOpen = Set.member key state.expandedItems
    jobs = case state.details of
      Just d -> d.workflowJobs
      Nothing -> Map.empty
    shaIndex = case state.details of
      Just d -> d.workflowShaIndex
      Nothing -> 0
    shaPRs = case state.details of
      Just d -> d.workflowShaPRs
      Nothing -> Map.empty
    shas = extractShas allRuns
    maxShas = 20
    currentSha = index shas shaIndex
    runs = case currentSha of
      Just sha -> runsForSha sha allRuns
      Nothing -> []
    allStatuses = map runStatus runs
    unique = sort $ Set.toUnfoldable
      $ Set.fromFoldable allStatuses
    statusLabels = map
      ( \s ->
          { name: s
          , count: length
              (filter (_ == s) allStatuses)
          }
      )
      unique
    filtered =
      if Set.isEmpty state.workflowStatusFilters then runs
      else filter
        ( \wr ->
            Set.member (runStatus wr)
              state.workflowStatusFilters
        )
        runs
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
                  <> "Workflows ("
                  <> show (min maxShas shaCount)
                  <> " commits)"
              )
          , if state.workflowsLoading then
              HH.span
                [ HP.class_
                    (HH.ClassName "loading")
                ]
                [ HH.text " \x21BB" ]
            else if not isOpen && shaCount == 0 then
              HH.text ""
            else refreshButton RefreshWorkflows
          ]
      , if not isOpen then HH.text ""
        else case currentSha of
          Nothing ->
            HH.div
              [ HP.class_
                  (HH.ClassName "empty-msg")
              ]
              [ HH.text "No workflow runs" ]
          Just sha ->
            HH.div_
              [ renderShaNav state shaIndex
                  (min maxShas (length shas))
                  sha
                  (Map.lookup sha shaPRs)
              , if not (null statusLabels) then
                  renderLabelSelector
                    state.workflowStatusFilters
                    ToggleWorkflowStatusFilter
                    statusLabels
                else HH.text ""
              , if null filtered then
                  HH.div
                    [ HP.class_
                        (HH.ClassName "empty-msg")
                    ]
                    [ HH.text
                        "All workflows hidden by filter"
                    ]
                else
                  HH.table
                    [ HP.class_
                        (HH.ClassName "detail-table")
                    ]
                    [ HH.thead_
                        [ HH.tr_
                            [ HH.th_ []
                            , HH.th_
                                [ HH.text "Workflow" ]
                            , HH.th_
                                [ HH.text "Status" ]
                            , HH.th_
                                [ HH.text "Date" ]
                            ]
                        ]
                    , HH.tbody_
                        ( filtered
                            >>= renderWorkflowRow
                              jobs
                        )
                    ]
              ]
      ]

-- | SHA navigation bar with prev/next and PR link.
renderShaNav
  :: forall w
   . State
  -> Int
  -> Int
  -> String
  -> Maybe CommitPR
  -> HH.HTML w Action
renderShaNav state idx total sha mPR =
  let
    fullName = fromMaybe "" state.expanded
    commitUrl = "https://github.com/" <> fullName
      <> "/commit/"
      <> sha
    shortSha = String.take 7 sha
  in
    HH.div
      [ HP.class_
          ( HH.ClassName
              "detail-heading"
          )
      ]
      ( [ HH.button
            [ HE.onClick \_ -> WorkflowPrevSha
            , HP.class_ (HH.ClassName "btn-small")
            , HP.disabled (idx <= 0)
            ]
            [ HH.text "\x25C0" ]
        , HH.text " "
        , HH.a
            [ HP.href commitUrl
            , HP.target "_blank"
            , HP.class_
                (HH.ClassName "detail-link")
            ]
            [ HH.text shortSha ]
        , HH.text
            ( " (" <> show (idx + 1) <> "/"
                <> show total
                <> ") "
            )
        , HH.button
            [ HE.onClick \_ -> WorkflowNextSha
            , HP.class_ (HH.ClassName "btn-small")
            , HP.disabled (idx >= total - 1)
            ]
            [ HH.text "\x25B6" ]
        ]
          <> case mPR of
            Nothing -> []
            Just pr ->
              [ HH.text " "
              , HH.a
                  [ HP.href pr.htmlUrl
                  , HP.target "_blank"
                  , HP.class_
                      (HH.ClassName "detail-link")
                  ]
                  [ HH.text pr.title ]
              ]
      )

-- | Single workflow run row + failed job rows.
renderWorkflowRow
  :: forall w
   . Map.Map String (Array WorkflowJob)
  -> WorkflowRun
  -> Array (HH.HTML w Action)
renderWorkflowRow jobs (WorkflowRun run) =
  let
    st = fromMaybe run.status run.conclusion
    cssClass = statusCssClass st
    runJobs = fromMaybe []
      (Map.lookup run.name jobs)
  in
    [ HH.tr
        [ HP.class_ (HH.ClassName "repo-row") ]
        [ HH.td_ [ linkButton run.htmlUrl ]
        , HH.td_
            [ HH.text run.name ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    ( HH.ClassName
                        ( "label-tag ci-badge "
                            <> cssClass
                        )
                    )
                ]
                [ HH.text st ]
            ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    (HH.ClassName "detail-date")
                ]
                [ HH.text
                    (formatDateTime run.updatedAt)
                ]
            ]
        ]
    ]
      <>
        if null runJobs then []
        else
          [ HH.tr
              [ HP.class_
                  (HH.ClassName "detail-row")
              ]
              [ HH.td
                  [ HP.colSpan 4 ]
                  [ HH.div
                      [ HP.class_
                          ( HH.ClassName
                              "check-runs"
                          )
                      ]
                      (map renderJobRow runJobs)
                  ]
              ]
          ]

-- | Single job row within a workflow.
renderJobRow
  :: forall w i. WorkflowJob -> HH.HTML w i
renderJobRow (WorkflowJob job) =
  let
    st = fromMaybe job.status job.conclusion
    cssClass = statusCssClass st
  in
    HH.div
      [ HP.class_
          (HH.ClassName "check-run")
      ]
      [ HH.span
          [ HP.class_
              ( HH.ClassName
                  ( "label-tag ci-badge "
                      <> cssClass
                  )
              )
          ]
          [ HH.text st ]
      , HH.a
          [ HP.href job.htmlUrl
          , HP.target "_blank"
          , HP.class_
              ( HH.ClassName
                  "detail-link check-name"
              )
          ]
          [ HH.text job.name ]
      ]

-- | Map status/conclusion to CSS class.
statusCssClass :: String -> String
statusCssClass = case _ of
  "success" -> "ci-success"
  "failure" -> "ci-failure"
  "cancelled" -> "ci-cancelled"
  _ -> "ci-pending"
