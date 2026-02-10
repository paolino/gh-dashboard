-- | Detail panel rendering — composes issues and PRs sections.
module View.Detail
  ( renderDetailPanel
  ) where

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import View.Issues (renderIssuesSection)
import View.PRs (renderPRsSection)
import View.Types (Action, State)
import View.Workflows (renderWorkflowsSection)

-- | Detail panel shown below expanded row.
renderDetailPanel
  :: forall w. State -> HH.HTML w Action
renderDetailPanel state =
  HH.tr
    [ HP.class_ (HH.ClassName "detail-panel") ]
    [ HH.td
        [ HP.colSpan 10 ]
        [ if state.detailLoading then
            HH.div
              [ HP.class_
                  (HH.ClassName "loading-spinner")
              ]
              [ HH.text
                  "Loading issues and PRs..."
              ]
          else case state.details of
            Nothing ->
              HH.div_
                [ renderWorkflowsSection state
                    []
                    0
                , renderIssuesSection state [] 0
                , renderPRsSection state [] 0
                ]
            Just detail ->
              HH.div_
                [ renderWorkflowsSection state
                    detail.workflowRuns
                    detail.workflowCount
                , renderIssuesSection state
                    detail.issues
                    detail.issueCount
                , renderPRsSection state
                    detail.pullRequests
                    detail.prCount
                ]
        ]
    ]
