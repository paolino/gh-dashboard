-- | GitHub REST API client with pagination support.
module GitHub
  ( RateLimit
  , ghFetch
  , fetchAllPages
  , fetchUserRepos
  , fetchRepoIssues
  , fetchIssue
  , fetchRepoPRs
  , fetchPR
  , fetchRepo
  , fetchCheckRuns
  , fetchCommitStatuses
  , fetchWorkflowRuns
  , fetchWorkflowJobs
  ) where

import Prelude

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  , decodeJson
  )
import Data.Argonaut.Decode.Combinators ((.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (catMaybes, nubByEq)
import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (Pattern(..), indexOf, drop, take)
import Effect.Aff (Aff, try)
import Effect.Exception (message)
import Fetch (fetch)
import Fetch.Internal.Headers as Headers
import Data.Number.Format (toString) as Number
import Types
  ( CheckRun(..)
  , Issue
  , PullRequest
  , Repo
  , WorkflowJob
  , WorkflowRun(..)
  )

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

-- | Fetch a single issue by number.
fetchIssue
  :: String
  -> String
  -> Int
  -> Aff (Either String Issue)
fetchIssue token fullName number = do
  result <- ghFetch token
    ( "https://api.github.com/repos/"
        <> fullName
        <> "/issues/"
        <> show number
    )
  pure $ result >>= \r ->
    case decodeJson r.json of
      Left err -> Left (show err)
      Right issue -> Right issue

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

-- | Fetch a single pull request by number.
fetchPR
  :: String
  -> String
  -> Int
  -> Aff (Either String PullRequest)
fetchPR token fullName number = do
  result <- ghFetch token
    ( "https://api.github.com/repos/"
        <> fullName
        <> "/pulls/"
        <> show number
    )
  pure $ result >>= \r ->
    case decodeJson r.json of
      Left err -> Left (show err)
      Right pr -> Right pr

-- | Fetch check-runs for a SHA.
fetchCheckRuns
  :: String
  -> String
  -> String
  -> Aff (Either String (Array CheckRun))
fetchCheckRuns token fullName sha = do
  result <- ghFetch token
    ( "https://api.github.com/repos/"
        <> fullName
        <> "/commits/"
        <> sha
        <> "/check-runs"
    )
  pure $ result >>= \r ->
    case decodeJson r.json of
      Left err -> Left (show err)
      Right
        ( res
            :: { check_runs ::
                   Array CheckRun
               }
        ) -> Right res.check_runs

-- | Fetch legacy commit statuses for a SHA, as CheckRun.
fetchCommitStatuses
  :: String
  -> String
  -> String
  -> Aff (Either String (Array CheckRun))
fetchCommitStatuses token fullName sha = do
  result <- ghFetch token
    ( "https://api.github.com/repos/"
        <> fullName
        <> "/commits/"
        <> sha
        <> "/statuses"
    )
  pure $ result >>= \r ->
    case decodeJson r.json of
      Left err -> Left (show err)
      Right
        ( statuses
            :: Array
                 { state :: String
                 , context :: String
                 , target_url :: Maybe String
                 }
        ) ->
        let
          unique = nubByEq
            (\a b -> a.context == b.context)
            statuses
        in
          Right $ map toCheckRun unique
  where
  toCheckRun s
    | s.state == "pending" = CheckRun
        { name: s.context
        , status: "in_progress"
        , conclusion: Nothing
        , htmlUrl: fromMaybe "" s.target_url
        }
    | otherwise = CheckRun
        { name: s.context
        , status: "completed"
        , conclusion: Just
            (mapState s.state)
        , htmlUrl: fromMaybe "" s.target_url
        }
  mapState "success" = "success"
  mapState "failure" = "failure"
  mapState "error" = "failure"
  mapState other = other

-- | Fetch latest workflow run per workflow on the default branch.
fetchWorkflowRuns
  :: String
  -> String
  -> String
  -> Aff (Either String (Array WorkflowRun))
fetchWorkflowRuns token fullName branch = do
  result <- ghFetch token
    ( "https://api.github.com/repos/"
        <> fullName
        <> "/actions/runs?branch="
        <> branch
        <> "&per_page=100"
    )
  pure $ result >>= \r ->
    case decodeJson r.json of
      Left err -> Left (show err)
      Right
        ( res
            :: { workflow_runs ::
                   Array WorkflowRun
               }
        ) ->
        Right $ nubByEq
          ( \(WorkflowRun a) (WorkflowRun b) ->
              a.name == b.name
          )
          res.workflow_runs

-- | Fetch jobs for a workflow run.
fetchWorkflowJobs
  :: String
  -> String
  -> Number
  -> Aff (Either String (Array WorkflowJob))
fetchWorkflowJobs token fullName runId = do
  result <- ghFetch token
    ( "https://api.github.com/repos/"
        <> fullName
        <> "/actions/runs/"
        <> Number.toString runId
        <> "/jobs"
    )
  pure $ result >>= \r ->
    case decodeJson r.json of
      Left err -> Left (show err)
      Right
        ( res
            :: { jobs :: Array WorkflowJob }
        ) -> Right res.jobs
