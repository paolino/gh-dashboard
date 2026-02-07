-- | GitHub REST API client with pagination support.
module GitHub
  ( RateLimit
  , ghFetch
  , fetchAllPages
  , fetchUserRepos
  , fetchRepoIssues
  , fetchRepoPRs
  , fetchRepo
  , fetchCommitStatus
  ) where

import Prelude

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  , decodeJson
  )
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), isNothing)
import Data.String (Pattern(..), indexOf, drop, take)
import Effect.Aff (Aff, try)
import Effect.Exception (message)
import Fetch (fetch)
import Fetch.Internal.Headers as Headers
import Types (Issue, PullRequest, Repo)

-- | Rate limit info from GitHub response headers.
type RateLimit =
  { remaining :: Int
  , limit :: Int
  }

-- | Raw response from a GitHub API call.
type GHResponse =
  { json :: Json
  , rateLimit :: Maybe RateLimit
  , linkNext :: Maybe String
  }

-- | Fetch a full GitHub API URL with authentication.
ghFetch
  :: String
  -> String
  -> Aff (Either String GHResponse)
ghFetch token url = do
  result <- try do
    resp <- fetch url
      { headers:
          { "Accept": "application/vnd.github.v3+json"
          , "Authorization": "Bearer " <> token
          , "If-None-Match": ""
          }
      }
    body <- resp.text
    pure { body, headers: resp.headers }
  case result of
    Left err -> pure $ Left (message err)
    Right r -> case jsonParser r.body of
      Left e ->
        pure $ Left ("JSON parse error: " <> e)
      Right json ->
        let
          rl = do
            rem <-
              Headers.lookup "x-ratelimit-remaining"
                r.headers
                >>= Int.fromString
            lim <-
              Headers.lookup "x-ratelimit-limit"
                r.headers
                >>= Int.fromString
            Just { remaining: rem, limit: lim }
          next = Headers.lookup "link" r.headers
            >>= parseLinkNext
        in
          pure $ Right
            { json, rateLimit: rl, linkNext: next }

-- | Parse the Link header for the next page URL.
parseLinkNext :: String -> Maybe String
parseLinkNext header = go 0
  where
  go offset =
    let
      rest = drop offset header
    in
      case indexOf (Pattern "<") rest of
        Nothing -> Nothing
        Just openIdx ->
          case
            indexOf (Pattern ">")
              (drop openIdx rest)
            of
            Nothing -> Nothing
            Just closeIdx ->
              let
                url = take closeIdx
                  (drop (openIdx + 1) rest)
                afterClose = drop
                  (openIdx + closeIdx + 1)
                  rest
              in
                case
                  indexOf (Pattern "rel=\"next\"")
                    afterClose
                  of
                  Just relIdx
                    | relIdx < 20 -> Just url
                  _ -> go
                    ( offset + openIdx
                        + closeIdx
                        + 1
                    )

-- | Fetch all pages of a paginated endpoint.
fetchAllPages
  :: forall a
   . DecodeJson a
  => String
  -> String
  -> Aff
       ( Either String
           { items :: Array a
           , rateLimit :: Maybe RateLimit
           }
       )
fetchAllPages token url = go url [] Nothing
  where
  go currentUrl acc lastRL = do
    result <- ghFetch token currentUrl
    case result of
      Left err -> pure $ Left err
      Right r -> case decodeJson r.json of
        Left err -> pure $ Left (show err)
        Right items ->
          let
            newAcc = acc <> items
            newRL = case r.rateLimit of
              Just rl -> Just rl
              Nothing -> lastRL
          in
            case r.linkNext of
              Just next -> go next newAcc newRL
              Nothing -> pure $ Right
                { items: newAcc
                , rateLimit: newRL
                }

-- | Fetch all repos owned by the authenticated user.
fetchUserRepos
  :: String
  -> Aff
       ( Either String
           { repos :: Array Repo
           , rateLimit :: Maybe RateLimit
           }
       )
fetchUserRepos token = do
  result <- ghFetch token
    ( "https://api.github.com/user/repos"
        <> "?affiliation=owner,collaborator"
        <> "&sort=updated&per_page=100"
    )
  pure $ result >>= \r ->
    case decodeJson r.json of
      Left err -> Left (show err)
      Right repos -> Right
        { repos, rateLimit: r.rateLimit }

-- | Wrapper to detect PRs in the /issues endpoint.
newtype RawIssue = RawIssue
  { issue :: Issue
  , hasPR :: Maybe Json
  }

instance DecodeJson RawIssue where
  decodeJson json = case toObject json of
    Nothing -> Left (TypeMismatch "Object")
    Just obj -> do
      issue <- decodeJson json
      hasPR <- obj .:? "pull_request"
      pure $ RawIssue { issue, hasPR }

-- | Fetch open issues for a repo (excludes PRs).
fetchRepoIssues
  :: String
  -> String
  -> Aff (Either String (Array Issue))
fetchRepoIssues token fullName = do
  result <- fetchAllPages token
    ( "https://api.github.com/repos/"
        <> fullName
        <> "/issues?state=open&per_page=100"
    )
  pure $ map
    ( \r -> catMaybes $ map
        ( \(RawIssue ri) ->
            if isNothing ri.hasPR then
              Just ri.issue
            else Nothing
        )
        r.items
    )
    result

-- | Fetch a single repo by full name (owner/repo).
fetchRepo
  :: String
  -> String
  -> Aff (Either String Repo)
fetchRepo token fullName = do
  result <- ghFetch token
    ( "https://api.github.com/repos/"
        <> fullName
    )
  pure $ result >>= \r ->
    case decodeJson r.json of
      Left err -> Left (show err)
      Right repo -> Right repo

-- | Fetch open pull requests for a repo.
fetchRepoPRs
  :: String
  -> String
  -> Aff (Either String (Array PullRequest))
fetchRepoPRs token fullName = do
  result <- fetchAllPages token
    ( "https://api.github.com/repos/"
        <> fullName
        <> "/pulls?state=open&per_page=100"
    )
  pure $ map _.items result

-- | Fetch combined commit status for a SHA.
fetchCommitStatus
  :: String
  -> String
  -> String
  -> Aff (Either String String)
fetchCommitStatus token fullName sha = do
  result <- ghFetch token
    ( "https://api.github.com/repos/"
        <> fullName
        <> "/commits/"
        <> sha
        <> "/status"
    )
  pure $ result >>= \r ->
    case toObject r.json of
      Nothing -> Left "Expected object"
      Just obj -> case obj .: "state" of
        Left err -> Left (show err)
        Right st -> Right st
