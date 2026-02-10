-- | Workflows section rendering.
module View.Workflows
  ( renderWorkflowsSection
  ) where

import Prelude

import Data.Array (null)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (WorkflowJob(..), WorkflowRun(..))
import View.DetailWidgets (refreshButton)
import View.Helpers (formatDate, linkButton)
import View.Types (Action(..), State)

-- | Workflows sub-section with collapsible toggle.
renderWorkflowsSection
  :: forall w
   . State
  -> Array WorkflowRun
  -> Int
  -> HH.HTML w Action
renderWorkflowsSection state runs count =
  let
    key = "section-workflows"
    isOpen = Set.member key state.expandedItems
    jobs = case state.details of
      Just d -> d.workflowJobs
      Nothing -> Map.empty
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
                  <> show count
                  <> ")"
              )
          , if state.workflowsLoading then
              HH.span
                [ HP.class_
                    (HH.ClassName "loading")
                ]
                [ HH.text " \x21BB" ]
            else if not isOpen && count == 0 then
              HH.text ""
            else refreshButton RefreshWorkflows
          ]
      , if not isOpen then HH.text ""
        else if null runs then
          HH.div
            [ HP.class_ (HH.ClassName "empty-msg") ]
            [ HH.text "No workflow runs" ]
        else
          HH.table
            [ HP.class_
                (HH.ClassName "detail-table")
            ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th_ []
                    , HH.th_ [ HH.text "Workflow" ]
                    , HH.th_ [ HH.text "Status" ]
                    , HH.th_ [ HH.text "Date" ]
                    ]
                ]
            , HH.tbody_
                ( runs >>= renderWorkflowRow jobs
                )
            ]
      ]

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
                    (formatDate run.updatedAt)
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
