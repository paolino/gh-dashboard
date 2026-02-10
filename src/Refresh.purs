-- | Async refresh logic for repos and PRs.
module Refresh
  ( doRefresh
  , refreshSinglePR
  ) where

import Prelude

import Data.Array (null, take)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import GitHub
  ( fetchCheckRuns
  , fetchCommitStatuses
  , fetchPR
  , fetchRepo
  , fetchUserRepos
  )
import Halogen as H
import RepoUtils (orderRepos, upsertRepo)
import Storage (saveRepoList)
import Types (PullRequest(..), Repo(..))
import View.Types (Action, State)

-- | Fetch repos. If repoList is empty, seed from API.
doRefresh
  :: forall o
   . String
  -> H.HalogenM State Action () o Aff Unit
doRefresh token = do
  st <- H.get
  if null st.repoList then do
    result <- H.liftAff (fetchUserRepos token)
    case result of
      Left err ->
        H.modify_ _
          { error = Just err, loading = false }
      Right { repos, rateLimit } -> do
        let
          seeded = take 25 repos
          names = map
            (\(Repo r) -> r.fullName)
            seeded
        H.modify_ _
          { repos = seeded
          , repoList = names
          , rateLimit = rateLimit
          , loading = false
          , error = Nothing
          }
        liftEffect $ saveRepoList names
  else do
    traverse_
      ( \name -> do
          result <- H.liftAff (fetchRepo token name)
          case result of
            Left _ -> pure unit
            Right repo -> do
              st2 <- H.get
              let
                updated = upsertRepo repo st2.repos
              H.modify_ _
                { repos = orderRepos st2.repoList
                    updated
                }
      )
      st.repoList
    H.modify_ _ { loading = false }

-- | Re-fetch a single PR and its check runs.
refreshSinglePR
  :: forall o
   . String
  -> String
  -> Int
  -> H.HalogenM State Action () o Aff Unit
refreshSinglePR token fullName prNum = do
  prResult <- H.liftAff
    (fetchPR token fullName prNum)
  case prResult of
    Left _ -> pure unit
    Right newPR@(PullRequest pr) -> do
      cr <- H.liftAff $
        fetchCheckRuns token fullName pr.headSha
      cs <- H.liftAff $
        fetchCommitStatuses token fullName
          pr.headSha
      let
        runs = case cr of
          Right r -> r
          Left _ -> []
        statuses = case cs of
          Right s -> s
          Left _ -> []
        newChecks = runs <> statuses
      st <- H.get
      case st.details of
        Nothing ->
          H.modify_ _
            { details = Just
                { issues: []
                , pullRequests: [ newPR ]
                , issueCount: 0
                , prCount: 1
                , prChecks: Map.singleton prNum
                    newChecks
                , workflowRuns: []
                , workflowCount: 0
                , workflowJobs: Map.empty
                , workflowShaIndex: 0
                , workflowShaPRs: Map.empty
                }
            }
        Just detail ->
          let
            updated = map
              ( \(PullRequest p) ->
                  if p.number == prNum then newPR
                  else PullRequest p
              )
              detail.pullRequests
          in
            H.modify_ _
              { details = Just detail
                  { pullRequests = updated
                  , prChecks = Map.insert prNum
                      newChecks
                      detail.prChecks
                  }
              }
