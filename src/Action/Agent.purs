-- | Agent/terminal action handlers.
-- |
-- | This module manages the integration with an
-- | external "agent server" — a daemon that runs
-- | Claude Code sessions against GitHub issues.
-- |
-- | **Session lifecycle:**
-- | - `handleLaunchAgent`  — POST to create a session,
-- |   then open a WebSocket-backed xterm.js terminal.
-- | - `handleDetachAgent`  — close the terminal widget
-- |   without stopping the remote session.
-- | - `handleStopAgent`    — DELETE the remote session
-- |   and tear down the terminal.
-- |
-- | **Terminal management:**
-- | - Terminals are keyed by `"owner/repo#issue"`.
-- | - DOM element IDs are derived via `termElementId`.
-- | - `reattachTerminals` re-opens xterm instances
-- |   after Halogen re-renders (virtual DOM may
-- |   destroy the container div).
-- |
-- | **Polling:**
-- | - `handleRefreshAgentSessions` — GET /sessions and
-- |   update the session status map so the UI can show
-- |   running/stopped badges.
module Action.Agent
  ( handleLaunchAgent
  , handleDetachAgent
  , handleStopAgent
  , handleSetAgentServer
  , handleRefreshAgentSessions
  , handleToggleSessionFilter
  , reattachTerminals
  ) where

import Prelude

import Action.Common
  ( Dispatch
  , HalogenAction
  , termElementId
  , toggleSet
  )
import Data.Argonaut.Core
  ( Json
  , jsonEmptyObject
  , stringify
  , toArray
  , toObject
  )
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (index)
import Data.Either (Either(..), hush)
import Data.HTTP.Method (Method(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String
  ( Pattern(..)
  , Replacement(..)
  , indexOf
  , replace
  , split
  )
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, try)
import FFI.Dialog (confirmDialog)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import FFI.Terminal (attachTerminal, destroyTerminal)
import Fetch (fetch)
import Halogen as H
import Storage (saveAgentServer)
import View.Types (Action(..), State)

handleLaunchAgent
  :: forall o
   . Dispatch o
  -> String
  -> String
  -> Int
  -> HalogenAction o
handleLaunchAgent dispatch toggleKey fullName issueNum = do
  st <- H.get
  let
    parts = split (Pattern "/") fullName
    owner = case index parts 0 of
      Just o -> o
      Nothing -> ""
    name = case index parts 1 of
      Just n -> n
      Nothing -> ""
    server = st.agentServer
    itemKey = fullName <> "#"
      <> show issueNum
  if server == "" then
    H.modify_ _
      { error = Just
          "Set agent server URL first"
      }
  else do
    let
      body = encodeJson
        ( "repo"
            :=
              ( "owner" := owner
                  ~> "name" := name
                  ~> jsonEmptyObject
              )
            ~> "issue" := issueNum
            ~> jsonEmptyObject
        )
    result <- H.liftAff $ try do
      resp <- fetch
        (server <> "/sessions")
        { method: POST
        , headers:
            { "Content-Type":
                "application/json"
            }
        , body: stringify body
        }
      resp.text
    case result of
      Left err ->
        H.modify_ _
          { error = Just (message err) }
      Right _ -> do
        let
          wsProto =
            if
              indexOf (Pattern "https")
                server == Just 0 then "wss"
            else "ws"
          host =
            replace (Pattern "https://")
              (Replacement "")
              $ replace
                  (Pattern "http://")
                  (Replacement "")
                  server
          wsUrl = wsProto <> "://" <> host
            <> "/sessions/"
            <> name
            <> "-"
            <> show issueNum
            <> "/terminal"
        -- Expand the item so the terminal
        -- div appears in the DOM
        H.modify_ \s -> s
          { error = Nothing
          , launchedItems =
              Set.insert itemKey
                s.launchedItems
          , expandedItems =
              Set.insert toggleKey
                s.expandedItems
          , terminalKeys =
              Map.insert toggleKey itemKey
                s.terminalKeys
          , terminalUrls =
              Map.insert itemKey wsUrl
                s.terminalUrls
          }
        -- Re-attach all active terminals
        st2 <- H.get
        liftEffect $ reattachTerminals st2
        dispatch RefreshAgentSessions

handleDetachAgent
  :: forall o
   . String
  -> Int
  -> HalogenAction o
handleDetachAgent fullName issueNum = do
  let
    itemKey = fullName <> "#"
      <> show issueNum
    elemId = termElementId itemKey
  liftEffect $ destroyTerminal elemId
  H.modify_ \s -> s
    { launchedItems =
        Set.delete itemKey s.launchedItems
    , terminalUrls =
        Map.delete itemKey s.terminalUrls
    }

handleStopAgent
  :: forall o
   . Dispatch o
  -> String
  -> Int
  -> HalogenAction o
handleStopAgent dispatch fullName issueNum = do
  confirmed <- liftEffect $
    confirmDialog
      "Stop this agent session?"
  when confirmed do
    st <- H.get
    let
      parts = split (Pattern "/") fullName
      name = case index parts 1 of
        Just n -> n
        Nothing -> ""
      server = st.agentServer
      itemKey = fullName <> "#"
        <> show issueNum
      elemId = termElementId itemKey
      sid = name <> "-" <> show issueNum
    liftEffect $ destroyTerminal elemId
    H.modify_ \s -> s
      { launchedItems =
          Set.delete itemKey s.launchedItems
      , terminalUrls =
          Map.delete itemKey s.terminalUrls
      }
    when (server /= "") do
      result <- H.liftAff $ try do
        resp <- fetch
          ( server <> "/sessions/"
              <> sid
          )
          { method: DELETE
          , headers:
              { "Content-Type":
                  "application/json"
              }
          }
        resp.text
      case result of
        Left err ->
          H.modify_ _
            { error = Just (message err) }
        Right _ -> pure unit
      dispatch RefreshAgentSessions

handleSetAgentServer
  :: forall o. String -> HalogenAction o
handleSetAgentServer url = do
  H.modify_ _ { agentServer = url }
  liftEffect $ saveAgentServer url

handleRefreshAgentSessions
  :: forall o. HalogenAction o
handleRefreshAgentSessions = do
  st <- H.get
  when (st.agentServer /= "") do
    result <- H.liftAff $ try do
      resp <- fetch
        (st.agentServer <> "/sessions")
        { method: GET }
      resp.text
    case result of
      Left _ -> pure unit
      Right txt -> case jsonParser txt of
        Left _ -> pure unit
        Right json ->
          case toArray json of
            Nothing -> pure unit
            Just arr ->
              let
                entries = arr >>= \sj ->
                  case parseSession sj of
                    Nothing -> []
                    Just e -> [ e ]
              in
                H.modify_ _
                  { agentSessions =
                      Map.fromFoldable entries
                  }
    -- Fetch worktree presence independently
    wtResult <- H.liftAff $ try do
      resp <- fetch
        (st.agentServer <> "/worktrees")
        { method: GET }
      resp.text
    case wtResult of
      Left _ -> pure unit
      Right wtTxt -> case jsonParser wtTxt of
        Left _ -> pure unit
        Right wtJson ->
          case toArray wtJson of
            Nothing -> pure unit
            Just arr ->
              let
                keys = arr >>= \wj ->
                  case parseWorktreeKey wj of
                    Nothing -> []
                    Just k -> [ k ]
              in
                H.modify_ _
                  { agentWorktrees =
                      Set.fromFoldable keys
                  }

handleToggleSessionFilter
  :: forall o. String -> HalogenAction o
handleToggleSessionFilter label =
  H.modify_ \s -> s
    { sessionFilters =
        toggleSet label s.sessionFilters
    }

-- | Re-attach all active terminals. Called after
-- | state changes that may cause Halogen to
-- | recreate terminal container divs.
reattachTerminals :: State -> Effect Unit
reattachTerminals st =
  traverse_
    ( \itemKey ->
        case Map.lookup itemKey st.terminalUrls of
          Nothing -> pure unit
          Just wsUrl ->
            attachTerminal
              (termElementId itemKey)
              itemKey
              wsUrl
    )
    (Set.toUnfoldable st.launchedItems :: Array String)

-- | Parse a single agent session JSON object into
-- | a (key, state) tuple.
parseSession :: Json -> Maybe (Tuple String String)
parseSession json = do
  obj <- toObject json
  repoJson <- hush (obj .: "repo")
  repoObj <- toObject repoJson
  owner <- hush (repoObj .: "owner") :: Maybe String
  name <- hush (repoObj .: "name") :: Maybe String
  issue <- hush (obj .: "issue") :: Maybe Int
  let
    state = fromMaybe "unknown"
      (hush (obj .: "state") :: Maybe String)
  Just $ Tuple
    (owner <> "/" <> name <> "#" <> show issue)
    state

-- | Parse a worktree JSON object into a session key.
parseWorktreeKey :: Json -> Maybe String
parseWorktreeKey json = do
  obj <- toObject json
  repoJson <- hush (obj .: "repo")
  repoObj <- toObject repoJson
  owner <- hush (repoObj .: "owner") :: Maybe String
  name <- hush (repoObj .: "name") :: Maybe String
  issue <- hush (obj .: "issue") :: Maybe Int
  Just (owner <> "/" <> name <> "#" <> show issue)
