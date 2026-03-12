-- | Projects view — renders GitHub Projects v2 as
-- | expandable table rows with lazy item loading.
module View.Projects
  ( renderProjects
  ) where

import Prelude

import Data.Array (filter, length, null, sort)
import Data.Function (on)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Project(..), ProjectItem(..))
import View.DetailWidgets (renderLabelSelector, refreshButton)
import View.Helpers (linkButton, renderMarkdownRow)
import View.Types (Action(..), State)

-- | Render all projects as an expandable table.
renderProjects
  :: forall w. State -> HH.HTML w Action
renderProjects state =
  HH.div_
    [ HH.div
        [ HP.class_
            (HH.ClassName "detail-heading")
        ]
        [ if state.projectsLoading then
            HH.span
              [ HP.class_
                  (HH.ClassName "loading")
              ]
              [ HH.text " \x21BB" ]
          else
            refreshButton RefreshProjects
        ]
    , if state.projectsLoading then
        HH.div
          [ HP.class_
              (HH.ClassName "loading-spinner")
          ]
          [ HH.text "Loading projects..." ]
      else if null state.projects then
        HH.p
          [ HP.class_ (HH.ClassName "muted") ]
          [ HH.text "No projects found." ]
      else
        HH.table
          [ HP.class_
              (HH.ClassName "repo-table")
          ]
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ []
                  , HH.th_
                      [ HH.text "Project" ]
                  , HH.th_
                      [ HH.text "Items" ]
                  ]
              ]
          , HH.tbody_
              ( state.projects >>= \p ->
                  renderProjectRow state p
              )
          ]
    ]

-- | A single project row with expand/collapse.
renderProjectRow
  :: forall w
   . State
  -> Project
  -> Array (HH.HTML w Action)
renderProjectRow state (Project p) =
  let
    isExpanded =
      state.expandedProject == Just p.id
    rowClass =
      if isExpanded then "repo-row expanded"
      else "repo-row"
  in
    [ HH.tr
        [ HE.onClick \_ -> ExpandProject p.id
        , HP.class_ (HH.ClassName rowClass)
        ]
        [ HH.td_
            [ linkButton p.url
            , HH.button
                [ HE.onClick \_ ->
                    RefreshProjectItems p.id
                , HP.class_
                    (HH.ClassName "btn-hide")
                , HP.title "Refresh"
                , HP.attr (AttrName "onclick")
                    "event.stopPropagation()"
                ]
                [ HH.text "\x21BB" ]
            ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    (HH.ClassName "repo-name")
                ]
                [ HH.text p.title ]
            ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    ( HH.ClassName
                        "badge badge-issues"
                    )
                ]
                [ HH.text (show p.itemCount) ]
            ]
        ]
    ]
      <>
        if isExpanded then
          [ renderProjectDetail state p.id ]
        else []

-- | Detail panel for an expanded project.
renderProjectDetail
  :: forall w
   . State
  -> String
  -> HH.HTML w Action
renderProjectDetail state projectId =
  HH.tr
    [ HP.class_ (HH.ClassName "detail-panel") ]
    [ HH.td
        [ HP.colSpan 3 ]
        [ if state.projectItemsLoading then
            HH.div
              [ HP.class_
                  (HH.ClassName "loading-spinner")
              ]
              [ HH.text "Loading items..." ]
          else
            case
              Map.lookup projectId
                state.projectItems
              of
              Nothing ->
                HH.div
                  [ HP.class_
                      (HH.ClassName "empty-msg")
                  ]
                  [ HH.text "No items loaded" ]
              Just items ->
                let
                  allRepos = collectRepoNames items
                  filtered =
                    if
                      Set.isEmpty
                        state.projectRepoFilters then items
                    else filter
                      ( \(ProjectItem i) ->
                          Set.member
                            ( fromMaybe "(no repo)"
                                i.repoName
                            )
                            state.projectRepoFilters
                      )
                      items
                in
                  HH.div_
                    ( ( if null allRepos then []
                        else
                          [ renderLabelSelector
                              state.projectRepoFilters
                              ToggleProjectRepoFilter
                              allRepos
                          ]
                      )
                        <> map
                          ( \col ->
                              renderStatusSection
                                state
                                projectId
                                col.name
                                col.items
                          )
                          (groupByStatus filtered)
                    )
        ]
    ]

-- | Collect unique repo names with counts.
collectRepoNames
  :: Array ProjectItem
  -> Array { name :: String, count :: Int }
collectRepoNames items =
  let
    allNames = map
      ( \(ProjectItem i) ->
          fromMaybe "(no repo)" i.repoName
      )
      items
    unique = sort $ Set.toUnfoldable
      $ Set.fromFoldable allNames
  in
    Array.sortBy (flip compare `on` _.count) $ map
      ( \n ->
          { name: n
          , count: length (filter (_ == n) allNames)
          }
      )
      unique

-- | Column order for status values.
statusOrder :: Array String
statusOrder =
  [ "Todo"
  , "In Progress"
  , "Done"
  , "Stale"
  , "(no status)"
  ]

-- | Group items by status into columns.
groupByStatus
  :: Array ProjectItem
  -> Array
       { name :: String
       , items :: Array ProjectItem
       }
groupByStatus items =
  filter (\col -> not (null col.items))
    ( map
        ( \status ->
            { name: status
            , items: filter
                ( \(ProjectItem i) ->
                    fromMaybe "(no status)"
                      i.status
                      == status
                )
                items
            }
        )
        statusOrder
    )

-- | Collapsible status section within a project.
renderStatusSection
  :: forall w
   . State
  -> String
  -> String
  -> Array ProjectItem
  -> HH.HTML w Action
renderStatusSection state projId name items =
  let
    key = "proj-status-" <> projId <> "-"
      <> name
    isOpen = Set.member key state.expandedItems
    count = length items
  in
    HH.div
      [ HP.class_
          (HH.ClassName "detail-section")
      ]
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
                  <> name
                  <> " ("
                  <> show count
                  <> ")"
              )
          , refreshButton
              (RefreshProjectItems projId)
          ]
      , if not isOpen then HH.text ""
        else
          HH.table
            [ HP.class_
                (HH.ClassName "detail-table")
            ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th_ []
                    , HH.th_
                        [ HH.text "Title" ]
                    , HH.th_
                        [ HH.text "Repo" ]
                    ]
                ]
            , HH.tbody_
                ( items >>= renderItemRow state
                    projId
                )
            ]
      ]

-- | A single project item row, expandable for body.
renderItemRow
  :: forall w
   . State
  -> String
  -> ProjectItem
  -> Array (HH.HTML w Action)
renderItemRow state projId (ProjectItem item) =
  let
    key = "proj-item-" <> projId <> "-"
      <> item.title
    isOpen = Set.member key state.expandedItems
  in
    [ HH.tr
        [ HE.onClick \_ -> ToggleItem key
        , HP.class_ (HH.ClassName "repo-row")
        ]
        [ HH.td_
            ( case item.repoName, item.number of
                Just repo, Just n ->
                  [ refreshButton
                      ( RefreshProjectItem
                          projId
                          repo
                          n
                      )
                  ]
                _, _ -> []
                <>
                  case item.url of
                    Just url -> [ linkButton url ]
                    Nothing -> []
            )
        , HH.td_
            [ HH.span_
                [ HH.text
                    ( case item.number of
                        Just n ->
                          "#" <> show n <> " "
                        Nothing -> ""
                    )
                , case item.url of
                    Just url ->
                      HH.a
                        [ HP.href url
                        , HP.target "_blank"
                        , HP.class_
                            ( HH.ClassName
                                "detail-link"
                            )
                        ]
                        [ HH.text item.title ]
                    Nothing ->
                      HH.text item.title
                ]
            , if null item.labels then
                HH.text ""
              else
                HH.span
                  [ HP.class_
                      ( HH.ClassName
                          "detail-labels"
                      )
                  ]
                  ( map
                      ( \lbl ->
                          HH.span
                            [ HP.class_
                                ( HH.ClassName
                                    "label-tag"
                                )
                            ]
                            [ HH.text lbl ]
                      )
                      item.labels
                  )
            ]
        , HH.td_
            [ HH.span
                [ HP.class_
                    (HH.ClassName "repo-desc")
                ]
                [ HH.text
                    ( fromMaybe ""
                        item.repoName
                    )
                ]
            ]
        ]
    ]
      <>
        if isOpen then
          renderMarkdownRow item.body
        else []
