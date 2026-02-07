-- | LocalStorage helpers for persisting dashboard state.
module Storage
  ( loadToken
  , saveToken
  , loadRepoList
  , saveRepoList
  , loadHidden
  , saveHidden
  , loadTheme
  , saveTheme
  , clearAll
  ) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as Storage

storageKeyToken :: String
storageKeyToken = "gh-dashboard-token"

storageKeyRepos :: String
storageKeyRepos = "gh-dashboard-repos"

storageKeyHidden :: String
storageKeyHidden = "gh-dashboard-hidden"

storageKeyTheme :: String
storageKeyTheme = "gh-dashboard-dark-theme"

loadToken :: Effect String
loadToken = do
  w <- window
  s <- localStorage w
  fromMaybe "" <$> Storage.getItem storageKeyToken s

saveToken :: String -> Effect Unit
saveToken tok = do
  w <- window
  s <- localStorage w
  Storage.setItem storageKeyToken tok s

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

loadTheme :: Effect Boolean
loadTheme = do
  w <- window
  s <- localStorage w
  raw <- Storage.getItem storageKeyTheme s
  pure $ case raw of
    Just "false" -> false
    _ -> true

saveTheme :: Boolean -> Effect Unit
saveTheme dark = do
  w <- window
  s <- localStorage w
  Storage.setItem storageKeyTheme (show dark) s

clearAll :: Effect Unit
clearAll = do
  w <- window
  s <- localStorage w
  Storage.removeItem storageKeyToken s
  Storage.removeItem storageKeyRepos s
  Storage.removeItem storageKeyHidden s
  Storage.removeItem storageKeyTheme s

loadHidden :: Effect (Set.Set String)
loadHidden = do
  w <- window
  s <- localStorage w
  raw <- Storage.getItem storageKeyHidden s
  pure $ case raw of
    Nothing -> Set.empty
    Just str ->
      case
        jsonParser str
          >>= (lmap printJsonDecodeError <<< decodeJson)
        of
        Right (arr :: Array String) ->
          Set.fromFoldable arr
        Left _ -> Set.empty

saveHidden :: Set.Set String -> Effect Unit
saveHidden hidden = do
  w <- window
  s <- localStorage w
  let
    arr :: Array String
    arr = Set.toUnfoldable hidden
  Storage.setItem storageKeyHidden
    (stringify (encodeJson arr))
    s
