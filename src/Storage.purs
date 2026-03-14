-- | LocalStorage helpers for persisting dashboard state.
module Storage
  ( ViewState
  , loadToken
  , saveToken
  , loadRepoList
  , saveRepoList
  , loadViewState
  , saveViewState
  , defaultViewState
  , loadAgentServer
  , saveAgentServer
  , clearAll
  ) where

import Prelude

import Data.Argonaut.Core (stringify, toObject)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff)
import FFI.Storage as FFIStorage
import Foreign.Object as FO
import Data.Argonaut.Core as Json
import Promise.Aff (toAffE)
import Types (Page(..))
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as Storage

-- | Persisted view state — everything about what is visible.
type ViewState =
  { currentPage :: Page
  , expanded :: Maybe String
  , expandedProject :: Maybe String
  , expandedItems :: Set.Set String
  , filterText :: String
  , hiddenItems :: Set.Set String
  , darkTheme :: Boolean
  , issueLabelFilters :: Set.Set String
  , prLabelFilters :: Set.Set String
  , workflowStatusFilters :: Set.Set String
  , projectRepoFilters :: Set.Set String
  }

defaultViewState :: ViewState
defaultViewState =
  { currentPage: ReposPage
  , expanded: Nothing
  , expandedProject: Nothing
  , expandedItems: Set.empty
  , filterText: ""
  , hiddenItems: Set.empty
  , darkTheme: true
  , issueLabelFilters: Set.empty
  , prLabelFilters: Set.empty
  , workflowStatusFilters: Set.empty
  , projectRepoFilters: Set.empty
  }

storageKeyToken :: String
storageKeyToken = "gh-dashboard-token"

storageKeyRepos :: String
storageKeyRepos = "gh-dashboard-repos"

storageKeyView :: String
storageKeyView = "gh-dashboard-view"

-- | Load and decrypt the token from localStorage.
-- | Handles migration from pre-encryption plaintext tokens.
loadToken :: Aff String
loadToken = toAffE FFIStorage.loadTokenEncrypted

-- | Encrypt and store the token in localStorage.
saveToken :: String -> Aff Unit
saveToken tok = toAffE (FFIStorage.saveTokenEncrypted tok)

loadRepoList :: Effect (Array String)
loadRepoList = do
  w <- window
  s <- localStorage w
  raw <- Storage.getItem storageKeyRepos s
  pure $ case raw of
    Nothing -> []
    Just str ->
      case
        jsonParser str
          >>= (lmap printJsonDecodeError <<< decodeJson)
        of
        Right arr -> arr
        Left _ -> []

saveRepoList :: Array String -> Effect Unit
saveRepoList repos = do
  w <- window
  s <- localStorage w
  Storage.setItem storageKeyRepos
    (stringify (encodeJson repos))
    s

-- | Encode a Set as a JSON array of strings.
encodeSet :: Set.Set String -> Json.Json
encodeSet ss =
  let
    arr :: Array String
    arr = Set.toUnfoldable ss
  in
    encodeJson arr

-- | Decode a Set from a JSON array of strings.
decodeSet :: Json.Json -> Set.Set String
decodeSet json =
  case decodeJson json of
    Right (arr :: Array String) ->
      Set.fromFoldable arr
    Left _ -> Set.empty

-- | Save the full view state as a single JSON blob.
saveViewState :: ViewState -> Effect Unit
saveViewState vs = do
  w <- window
  s <- localStorage w
  let
    obj = FO.fromFoldable
      [ "currentPage" /\ encodeJson
          ( case vs.currentPage of
              ReposPage -> "ReposPage"
              ProjectsPage -> "ProjectsPage"
          )
      , "expanded" /\ encodeJson vs.expanded
      , "expandedProject" /\ encodeJson
          vs.expandedProject
      , "expandedItems" /\ encodeSet
          vs.expandedItems
      , "filterText" /\ encodeJson vs.filterText
      , "hiddenItems" /\ encodeSet vs.hiddenItems
      , "darkTheme" /\ encodeJson vs.darkTheme
      , "issueLabelFilters" /\ encodeSet
          vs.issueLabelFilters
      , "prLabelFilters" /\ encodeSet
          vs.prLabelFilters
      , "workflowStatusFilters" /\ encodeSet
          vs.workflowStatusFilters
      , "projectRepoFilters" /\ encodeSet
          vs.projectRepoFilters
      ]
  Storage.setItem storageKeyView
    (stringify (Json.fromObject obj))
    s

-- | Load view state from localStorage.
loadViewState :: Effect ViewState
loadViewState = do
  w <- window
  s <- localStorage w
  raw <- Storage.getItem storageKeyView s
  pure $ case raw of
    Nothing -> defaultViewState
    Just str ->
      case jsonParser str of
        Left _ -> defaultViewState
        Right json ->
          case toObject json of
            Nothing -> defaultViewState
            Just obj ->
              { currentPage:
                  case lmap printJsonDecodeError
                    (obj .: "currentPage") of
                    Right "ProjectsPage" ->
                      ProjectsPage
                    _ -> ReposPage
              , expanded:
                  case lmap printJsonDecodeError
                    (obj .:? "expanded") of
                    Right m -> m
                    _ -> Nothing
              , expandedProject:
                  case lmap printJsonDecodeError
                    (obj .:? "expandedProject") of
                    Right m -> m
                    _ -> Nothing
              , expandedItems:
                  case lmap printJsonDecodeError
                    (obj .: "expandedItems") of
                    Right j -> decodeSet j
                    _ -> Set.empty
              , filterText:
                  case lmap printJsonDecodeError
                    (obj .: "filterText") of
                    Right t -> t
                    _ -> ""
              , hiddenItems:
                  case lmap printJsonDecodeError
                    (obj .: "hiddenItems") of
                    Right j -> decodeSet j
                    _ -> Set.empty
              , darkTheme:
                  case lmap printJsonDecodeError
                    (obj .: "darkTheme") of
                    Right d -> d
                    _ -> true
              , issueLabelFilters:
                  case lmap printJsonDecodeError
                    (obj .: "issueLabelFilters") of
                    Right j -> decodeSet j
                    _ -> Set.empty
              , prLabelFilters:
                  case lmap printJsonDecodeError
                    (obj .: "prLabelFilters") of
                    Right j -> decodeSet j
                    _ -> Set.empty
              , workflowStatusFilters:
                  case lmap printJsonDecodeError
                    (obj .: "workflowStatusFilters") of
                    Right j -> decodeSet j
                    _ -> Set.empty
              , projectRepoFilters:
                  case lmap printJsonDecodeError
                    (obj .: "projectRepoFilters") of
                    Right j -> decodeSet j
                    _ -> Set.empty
              }

storageKeyAgentServer :: String
storageKeyAgentServer = "gh-dashboard-agent-server"

loadAgentServer :: Effect String
loadAgentServer = do
  w <- window
  s <- localStorage w
  fromMaybe "" <$> Storage.getItem storageKeyAgentServer s

saveAgentServer :: String -> Effect Unit
saveAgentServer url = do
  w <- window
  s <- localStorage w
  Storage.setItem storageKeyAgentServer url s

storageKeyCryptoKey :: String
storageKeyCryptoKey = "gh-dashboard-crypto-key"

clearAll :: Effect Unit
clearAll = do
  w <- window
  s <- localStorage w
  Storage.removeItem storageKeyToken s
  Storage.removeItem storageKeyRepos s
  Storage.removeItem storageKeyView s
  Storage.removeItem storageKeyCryptoKey s
