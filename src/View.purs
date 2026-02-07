-- | View — top-level render functions and shared types.
module View
  ( module View.Types
  , renderTokenForm
  , renderDashboard
  ) where

import Prelude

import Data.Array (null)
import Data.Maybe (Maybe(..))
import GitHub (RateLimit)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Repo)
import View.RepoTable (renderRepoTable)
import View.Types (Action(..), State)

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

-- | Toolbar with filter and controls.
renderToolbar
  :: forall w. State -> HH.HTML w Action
renderToolbar state =
  HH.div
    [ HP.class_ (HH.ClassName "toolbar") ]
    [ HH.button
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
    , renderRateLimit state.rateLimit
    , HH.div
        [ HP.class_
            (HH.ClassName "toolbar-spacer")
        ]
        []
    , HH.a
        [ HP.href
            "https://github.com/paolino/gh-dashboard"
        , HP.target "_blank"
        , HP.class_ (HH.ClassName "link-btn")
        ]
        [ HH.img
            [ HP.src
                "https://github.githubassets.com/favicons/favicon-dark.svg"
            , HP.width 16
            , HP.height 16
            ]
        ]
    , HH.button
        [ HE.onClick \_ -> ResetAll
        , HP.class_ (HH.ClassName "btn-hide")
        ]
        [ HH.text "\x2620" ]
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
