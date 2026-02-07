-- | Pure helpers for repo list manipulation.
module RepoUtils
  ( applyFilter
  , parseRepoName
  , upsertRepo
  , orderRepos
  , moveItem
  ) where

import Prelude

import Data.Array (filter, findIndex)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, toLower, trim)
import Data.String (replaceAll, split) as Str
import Data.String.Pattern (Replacement(..))
import Types (Repo(..))

-- | Filter repos by name/description.
applyFilter :: String -> Array Repo -> Array Repo
applyFilter filterText repos
  | filterText == "" = repos
  | otherwise =
      let
        q = toLower filterText
      in
        filter
          ( \(Repo r) ->
              contains (Pattern q) (toLower r.name)
                || contains (Pattern q)
                  (toLower (fromMaybe "" r.description))
          )
          repos

-- | Extract owner/repo from a GitHub URL or plain name.
parseRepoName :: String -> Maybe String
parseRepoName input =
  let
    stripped = Str.replaceAll
      (Pattern "https://github.com/")
      (Replacement "")
      ( Str.replaceAll
          (Pattern "http://github.com/")
          (Replacement "")
          (trim input)
      )
    parts = filter (_ /= "")
      (Str.split (Pattern "/") stripped)
  in
    case parts of
      [ owner, repo ] -> Just (owner <> "/" <> repo)
      _ -> Nothing

-- | Insert or update a repo in the array.
upsertRepo :: Repo -> Array Repo -> Array Repo
upsertRepo repo@(Repo r) repos =
  if Array.any
    (\(Repo x) -> x.fullName == r.fullName)
    repos
  then
    map
      ( \(Repo x) ->
          if x.fullName == r.fullName then repo
          else Repo x
      )
      repos
  else Array.snoc repos repo

-- | Order repos to match the stored list.
orderRepos :: Array String -> Array Repo -> Array Repo
orderRepos order repos = Array.catMaybes $ map
  ( \name -> Array.find
      (\(Repo r) -> r.fullName == name)
      repos
  )
  order

-- | Move an item before another in the list.
moveItem :: String -> String -> Array String -> Array String
moveItem src target order =
  let
    without = filter (_ /= src) order
  in
    case findIndex (_ == target) without of
      Nothing -> order
      Just idx ->
        fromMaybe order
          (Array.insertAt idx src without)
