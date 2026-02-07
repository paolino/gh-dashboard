-- | View — all render functions for the dashboard.
module View
  ( Action(..)
  , State
  , renderTokenForm
  , renderDashboard
  ) where

import Prelude

import Data.Array (null)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith, take)
import GitHub (RateLimit)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), PropName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types
  ( Assignee
  , Issue(..)
  , Label
  , PullRequest(..)
  , Repo(..)
  , RepoDetail
  )

foreign import parseMarkdownImpl :: String -> String

-- | Actions emitted by the view.
data Action
  = Initialize
  | Tick
  | SetToken String
  | SubmitToken
  | Refresh
  | ToggleExpand String
  | ToggleItem String
  | SetFilter String
  | ToggleAutoRefresh
  | ChangeInterval Int
  | DragStart String
  | DragDrop String
  | ToggleAddRepo
  | SetAddRepoInput String
  | SubmitAddRepo
  | RemoveRepo String
  | ResetAll

-- | Application state (referenced by view).
type State =
  { token :: String
  , repos :: Array Repo
  , expanded :: Maybe String
  , details :: Maybe RepoDetail
  , detailLoading :: Boolean
  , loading :: Boolean
  , error :: Maybe String
  , rateLimit :: Maybe RateLimit
  , interval :: Int
  , secondsLeft :: Int
  , filterText :: String
  , hasToken :: Boolean
  , expandedItems :: Set String
  , autoRefresh :: Boolean
  , repoList :: Array String
  , dragging :: Maybe String
  , showAddRepo :: Boolean
  , addRepoInput :: String
  }

-- | Token input form shown when no token is set.
renderTokenForm
  :: forall w
   . State
  -> HH.HTML w Action
renderTokenForm state =
  HH.div
    [ HP.class_ (HH.ClassName "form-container") ]
    [ HH.h1_ [ HH.text "GH Dashboard" ]
    , HH.p
        [ HP.class_ (HH.ClassName "muted") ]
        [ HH.text
            "Your GitHub repositories at a glance"
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "form") ]
        [ HH.input
            [ HP.type_ HP.InputPassword
            , HP.placeholder "GitHub personal access token"
            , HP.value state.token
            , HE.onValueInput SetToken
            , HP.class_ (HH.ClassName "input")
            ]
        , HH.button
            [ HE.onClick \_ -> SubmitToken
            , HP.class_ (HH.ClassName "btn")
            ]
            [ HH.text "Connect" ]
        ]
    , case state.error of
        Just err ->
          HH.div
            [ HP.class_ (HH.ClassName "error") ]
            [ HH.text err ]
        Nothing -> HH.text ""
    , HH.div
        [ HP.class_ (HH.ClassName "instructions") ]
        [ HH.h3_ [ HH.text "Getting started" ]
        , HH.ol_
            [ HH.li_
                [ HH.a
                    [ HP.href
                        "https://github.com/settings/tokens/new?scopes=repo&description=gh-dashboard"
                    , HP.target "_blank"
                    , HP.class_
                        (HH.ClassName "token-link")
                    ]
                    [ HH.text
                        "Create a GitHub token"
                    ]
                , HH.text " (select "
                , HH.code_ [ HH.text "repo" ]
                , HH.text " scope)"
                ]
            , HH.li_
                [ HH.text
                    "Paste it above and click Connect"
                ]
            , HH.li_
                [ HH.text
                    "Browse your repos, expand for issues and PRs"
                ]
            ]
        ]
    ]

-- | Full dashboard view with toolbar and repo table.
renderDashboard
  :: forall w
   . State
  -> Array Repo
  -> HH.HTML w Action
renderDashboard state repos =
  HH.div_
    [ renderToolbar state
    , if state.showAddRepo then
        HH.div
          [ HP.class_ (HH.ClassName "add-repo-bar") ]
          [ HH.input
              [ HP.placeholder
                  "https://github.com/owner/repo"
              , HP.value state.addRepoInput
              , HE.onValueInput SetAddRepoInput
              , HP.class_
                  (HH.ClassName "filter-input")
              ]
          , HH.button
              [ HE.onClick \_ -> SubmitAddRepo
              , HP.class_ (HH.ClassName "btn-small")
              ]
              [ HH.text "Add" ]
          ]
      else HH.text ""
    , case state.error of
        Just err ->
          HH.div
            [ HP.class_ (HH.ClassName "error") ]
            [ HH.text err ]
        Nothing -> HH.text ""
    , if null repos then
        HH.p
          [ HP.class_ (HH.ClassName "muted") ]
          [ HH.text "No repositories found." ]
      else
        renderRepoTable state repos
    ]

-- | Toolbar with refresh, timer, filter.
renderToolbar
  :: forall w. State -> HH.HTML w Action
renderToolbar state =
  HH.div
    [ HP.class_ (HH.ClassName "toolbar") ]
    [ HH.button
        [ HE.onClick \_ -> Refresh
        , HP.class_ (HH.ClassName "btn-back")
        ]
        [ HH.text
            ( if state.loading then "\x21BB ..."
              else "\x21BB Refresh"
            )
        ]
    , HH.button
        [ HE.onClick \_ -> ToggleAddRepo
        , HP.class_
            ( HH.ClassName
                ( "btn-back"
                    <> activeIf state.showAddRepo
                )
            )
        ]
        [ HH.text "+" ]
    , HH.input
        [ HP.placeholder "Filter repos..."
        , HP.value state.filterText
        , HE.onValueInput SetFilter
        , HP.class_ (HH.ClassName "filter-input")
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "toolbar-timer") ]
        [ HH.button
            [ HE.onClick \_ -> ToggleAutoRefresh
            , HP.class_
                ( HH.ClassName
                    ( "btn-small"
                        <> activeIf state.autoRefresh
                    )
                )
            ]
            [ HH.text
                ( if state.autoRefresh then
                    "\x25B6 Auto"
                  else "\x23F8 Paused"
                )
            ]
        , HH.button
            [ HE.onClick \_ -> ChangeInterval (-5)
            , HP.class_ (HH.ClassName "btn-small")
            ]
            [ HH.text "-" ]
        , HH.text
            ( if state.autoRefresh then
                show state.secondsLeft <> "s / "
                  <> show state.interval
                  <> "s"
              else show state.interval <> "s"
            )
        , HH.button
            [ HE.onClick \_ -> ChangeInterval 5
            , HP.class_ (HH.ClassName "btn-small")
            ]
            [ HH.text "+" ]
        , renderRateLimit state.rateLimit
        ]
    , HH.a
        [ HP.href
            "https://github.com/paolino/gh-dashboard"
        , HP.target "_blank"
        , HP.class_ (HH.ClassName "btn-small")
        ]
        [ HH.text "\x2605 GitHub" ]
    , HH.button
        [ HE.onClick \_ -> ResetAll
        , HP.class_ (HH.ClassName "btn-small")
        ]
        [ HH.text "\x2715 Reset" ]
    ]

activeIf :: Boolean -> String
activeIf true = " active"
activeIf false = ""

-- | Rate limit display.
renderRateLimit
  :: forall w i. Maybe RateLimit -> HH.HTML w i
renderRateLimit = case _ of
  Nothing -> HH.text ""
  Just rl ->
    HH.span
      [ HP.class_
          ( HH.ClassName
              ( if rl.remaining < 100 then
                  "rate-limit rate-limit-warn"
                else "rate-limit"
              )
          )
      ]
      [ HH.text
          ( show rl.remaining <> "/"
              <> show rl.limit
          )
      ]

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
              HH.div
                [ HP.class_
                    (HH.ClassName "loading-spinner")
                ]
                [ HH.text "Loading..." ]
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

-- | Issues sub-section.
renderIssuesSection
  :: forall w
   . State
  -> Array Issue
  -> Int
  -> HH.HTML w Action
renderIssuesSection state issues count =
  let
    key = "section-issues"
    isOpen = Set.member key state.expandedItems
  in
    HH.div
      [ HP.class_ (HH.ClassName "detail-section") ]
      [ HH.div
          [ HP.class_
              (HH.ClassName "detail-heading clickable")
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
            [ detailHead
            , HH.tbody_
                ( issues >>= renderIssueRow state
                )
            ]
      ]

-- | Single issue row + optional body row.
renderIssueRow
  :: forall w
   . State
  -> Issue
  -> Array (HH.HTML w Action)
renderIssueRow state (Issue i) =
  let
    key = "issue-" <> show i.number
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
        ]
    ]
      <>
        if isOpen then
          renderMarkdownRow i.body
        else []

-- | PRs sub-section.
renderPRsSection
  :: forall w
   . State
  -> Array PullRequest
  -> Int
  -> HH.HTML w Action
renderPRsSection state prs count =
  let
    key = "section-prs"
    isOpen = Set.member key state.expandedItems
  in
    HH.div
      [ HP.class_ (HH.ClassName "detail-section") ]
      [ HH.div
          [ HP.class_
              (HH.ClassName "detail-heading clickable")
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
            [ detailHead
            , HH.tbody_
                (prs >>= renderPRRow state)
            ]
      ]

-- | Single PR row + optional body row.
renderPRRow
  :: forall w
   . State
  -> PullRequest
  -> Array (HH.HTML w Action)
renderPRRow state (PullRequest pr) =
  let
    key = "pr-" <> show pr.number
    isOpen = Set.member key state.expandedItems
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
                  if pr.draft then
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
        ]
    ]
      <>
        if isOpen then
          renderMarkdownRow pr.body
        else []

-- | Row with markdown-rendered body.
renderMarkdownRow
  :: forall w i
   . Maybe String
  -> Array (HH.HTML w i)
renderMarkdownRow = case _ of
  Nothing -> []
  Just "" -> []
  Just body ->
    [ HH.tr
        [ HP.class_ (HH.ClassName "detail-row") ]
        [ HH.td
            [ HP.colSpan 5 ]
            [ HH.div
                [ HP.class_
                    (HH.ClassName "detail-body")
                , HP.prop
                    ( PropName "innerHTML"
                        :: PropName String
                    )
                    (parseMarkdownImpl body)
                ]
                []
            ]
        ]
    ]

-- | Small link button that opens a URL.
linkButton :: forall w i. String -> HH.HTML w i
linkButton url =
  HH.a
    [ HP.href url
    , HP.target "_blank"
    , HP.class_ (HH.ClassName "link-btn")
    ]
    [ HH.text "\x2197" ]

-- | Column headers for detail tables.
detailHead :: forall w i. HH.HTML w i
detailHead =
  HH.thead_
    [ HH.tr_
        [ HH.th_ [ HH.text "Title" ]
        , HH.th_ [ HH.text "Assignees" ]
        , HH.th_ [ HH.text "Author" ]
        , HH.th_ [ HH.text "Date" ]
        , HH.th_ []
        ]
    ]

-- | Render assignees.
renderAssignees
  :: forall w i. Array Assignee -> HH.HTML w i
renderAssignees assignees =
  if null assignees then HH.text ""
  else
    HH.span
      [ HP.class_
          (HH.ClassName "detail-assignees")
      ]
      [ HH.text
          (joinWith ", " (map _.login assignees))
      ]

-- | Render label tags.
renderLabels
  :: forall w i. Array Label -> HH.HTML w i
renderLabels labels =
  if null labels then HH.text ""
  else
    HH.span
      [ HP.class_
          (HH.ClassName "detail-labels")
      ]
      ( map
          ( \l ->
              HH.span
                [ HP.class_
                    (HH.ClassName "label-tag")
                ]
                [ HH.text l.name ]
          )
          labels
      )

-- | Format ISO date to short form (YYYY-MM-DD).
formatDate :: String -> String
formatDate = take 10
