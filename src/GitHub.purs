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
  , fetchCommitPRs
  , fetchUserProjects
  , fetchProjectItems
  , updateItemStatus
  , addDraftItem
  , updateDraftItem
  ) where

import Prelude

import Data.Argonaut.Core
  ( Json
  , jsonEmptyObject
  , stringify
  , toObject
  )
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  , decodeJson
  )
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (catMaybes, mapMaybe, nubByEq)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.HTTP.Method (Method(..))
import Data.String
  ( Pattern(..), indexOf, drop, take, replaceAll
  )
import Data.String.Pattern (Replacement(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff, try)
import Effect.Exception (message)
import Fetch (fetch)
import Fetch.Internal.Headers as Headers
import Data.Number.Format (toString) as Number
import Data.Array (head) as Array
import Types
  ( CheckRun(..)
  , CommitPR
  , Issue
  , Project(..)
  , ProjectItem(..)
  , StatusField
  , PullRequest
  , Repo
  , WorkflowJob
  , WorkflowRun
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
        Right res.workflow_runs

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

-- | Fetch the first PR associated with a commit SHA.
fetchCommitPRs
  :: String
  -> String
  -> String
  -> Aff (Either String (Maybe CommitPR))
fetchCommitPRs token fullName sha = do
  result <- ghFetch token
    ( "https://api.github.com/repos/"
        <> fullName
        <> "/commits/"
        <> sha
        <> "/pulls"
    )
  pure $ result >>= \r ->
    case decodeJson r.json of
      Left err -> Left (show err)
      Right
        ( prs
            :: Array
                 { title :: String
                 , html_url :: String
                 }
        ) -> Right $ Array.head prs <#>
        \p ->
          { title: p.title
          , htmlUrl: p.html_url
          }

-- | POST a GraphQL query to the GitHub API.
ghGraphQL
  :: String
  -> String
  -> Aff (Either String Json)
ghGraphQL token query = do
  result <- try do
    let
      body = encodeJson
        ( "query" := query
            ~> jsonEmptyObject
        )
    resp <- fetch "https://api.github.com/graphql"
      { method: POST
      , headers:
          { "Accept":
              "application/vnd.github.v3+json"
          , "Authorization": "Bearer " <> token
          , "Content-Type": "application/json"
          }
      , body: stringify body
      }
    resp.text
  case result of
    Left err -> pure $ Left (message err)
    Right txt -> case jsonParser txt of
      Left e ->
        pure $ Left ("JSON parse error: " <> e)
      Right json -> pure $ Right json

-- | Light query: project list with item counts.
projectsListQuery :: String
projectsListQuery =
  """
  query {
    viewer {
      projectsV2(first: 20) {
        nodes {
          id
          title
          url
          items { totalCount }
        }
      }
    }
  }
  """

-- | Detail query: items for a single project.
projectItemsQuery
  :: String -> Maybe String -> String
projectItemsQuery nodeId mCursor =
  let
    afterArg = case mCursor of
      Nothing -> ""
      Just c -> " after: \"" <> c <> "\""
  in
    "query { node(id: \"" <> nodeId <> "\") {"
      <> " ... on ProjectV2 {"
      <> " fields(first: 20) { nodes {"
      <> " ... on ProjectV2SingleSelectField {"
      <> " id name options { id name }"
      <> " } } }"
      <> " items(first: 100" <> afterArg <> ") {"
      <> " pageInfo { hasNextPage endCursor }"
      <> " nodes { id fieldValues(first: 10) { nodes {"
    <> " ... on ProjectV2ItemFieldSingleSelectValue"
    <> " { name field {"
    <> " ... on ProjectV2SingleSelectField"
    <> " { name } } }"
    <> " ... on ProjectV2ItemFieldLabelValue"
    <> " { labels(first: 10) { nodes { name } }"
    <> " field {"
    <> " ... on ProjectV2FieldCommon"
    <> " { name } } }"
    <> " } }"
    <> " content {"
    <> " ... on Issue { title url number body"
    <> " repository { nameWithOwner } }"
    <> " ... on PullRequest { title url number"
    <> " body repository { nameWithOwner } }"
    <> " ... on DraftIssue { id title body }"
    <> " } } } } } }"

-- | Fetch the authenticated user's Projects v2
-- | (light: no items, just metadata).
fetchUserProjects
  :: String
  -> Aff (Either String (Array Project))
fetchUserProjects token = do
  result <- ghGraphQL token projectsListQuery
  case result of
    Left err -> pure $ Left err
    Right json ->
      pure $ navigateProjects json

-- | Navigate the GraphQL response to project nodes.
navigateProjects
  :: Json -> Either String (Array Project)
navigateProjects json = do
  obj <- note "Expected object"
    (toObject json)
  dataJson <- lmap show $ obj .: "data"
  dataObj <- note "Expected data object"
    (toObject dataJson)
  viewerJson <- lmap show $
    dataObj .: "viewer"
  viewerObj <- note "Expected viewer object"
    (toObject viewerJson)
  projsJson <- lmap show $
    viewerObj .: "projectsV2"
  projsObj <- note "Expected projects object"
    (toObject projsJson)
  nodes <- lmap show $ projsObj .: "nodes"
  traverse parseProject nodes

-- | Parse a single project node (light).
parseProject :: Json -> Either String Project
parseProject json = case toObject json of
  Nothing -> Left "Expected project object"
  Just obj -> do
    id_ <- lmap show $ obj .: "id"
    title_ <- lmap show $ obj .: "title"
    url_ <- lmap show $ obj .: "url"
    itemsJson <- lmap show $ obj .: "items"
    case toObject itemsJson of
      Nothing -> Left "Expected items object"
      Just itemsObj -> do
        count <- lmap show $
          itemsObj .: "totalCount"
        Right $ Project
          { id: id_
          , title: title_
          , url: url_
          , itemCount: count
          }

-- | Fetch items for a single project (paginated).
fetchProjectItems
  :: String
  -> String
  -> Aff
       ( Either String
           { items :: Array ProjectItem
           , statusField :: Maybe StatusField
           }
       )
fetchProjectItems token projectId =
  fetchPage Nothing []
  where
  fetchPage mCursor acc = do
    result <- ghGraphQL token
      (projectItemsQuery projectId mCursor)
    case result of
      Left err -> pure $ Left err
      Right json ->
        case navigateProjectItems json of
          Left err -> pure $ Left err
          Right page ->
            let all = acc <> page.items
            in
              if page.hasNextPage then
                fetchPage page.endCursor all
              else
                pure $ Right
                  { items: all
                  , statusField: page.statusField
                  }

-- | Navigate GraphQL response to one page.
navigateProjectItems
  :: Json
  -> Either String
       { items :: Array ProjectItem
       , statusField :: Maybe StatusField
       , hasNextPage :: Boolean
       , endCursor :: Maybe String
       }
navigateProjectItems json = do
  obj <- note "Expected object"
    (toObject json)
  dataJson <- lmap show $ obj .: "data"
  dataObj <- note "Expected data object"
    (toObject dataJson)
  nodeJson <- lmap show $ dataObj .: "node"
  nodeObj <- note "Expected node object"
    (toObject nodeJson)
  let statusField_ = parseStatusField nodeJson
  itemsJson <- lmap show $
    nodeObj .: "items"
  itemsObj <- note "Expected items object"
    (toObject itemsJson)
  pageInfoJson <- lmap show $
    itemsObj .: "pageInfo"
  pageInfoObj <- note "Expected pageInfo object"
    (toObject pageInfoJson)
  hasNext <- lmap show $
    pageInfoObj .: "hasNextPage"
  let
    endCursor_ = case
      pageInfoObj .: "endCursor" of
      Right c -> Just c
      Left _ -> Nothing
  itemNodes <- lmap show $
    itemsObj .: "nodes"
  items <- traverse parseProjectItem itemNodes
  Right
    { items: catMaybes items
    , statusField: statusField_
    , hasNextPage: hasNext
    , endCursor: endCursor_
    }

-- | Helper: convert Maybe to Either.
note :: forall a. String -> Maybe a -> Either String a
note msg Nothing = Left msg
note _ (Just a) = Right a

-- | Parse a single project item node.
parseProjectItem
  :: Json -> Either String (Maybe ProjectItem)
parseProjectItem json = case toObject json of
  Nothing -> Left "Expected item object"
  Just obj -> do
    itemId_ <- lmap show $ obj .: "id"
    contentJson <- lmap show $ obj .: "content"
    case toObject contentJson of
      Nothing -> Right Nothing
      Just contentObj ->
        case contentObj .: "title" of
          Left _ -> Right Nothing
          Right title_ -> do
            let
              url_ = case contentObj .: "url" of
                Right u -> Just u
                Left _ -> Nothing
              number_ = case
                contentObj .: "number" of
                Right n -> Just n
                Left _ -> Nothing
              body_ = case contentObj .: "body" of
                Right b -> Just b
                Left _ -> Nothing
              repoName_ = case
                contentObj .: "repository" of
                Right repoJson ->
                  case toObject repoJson of
                    Just repoObj ->
                      case
                        repoObj .: "nameWithOwner"
                        of
                        Right n -> Just n
                        Left _ -> Nothing
                    Nothing -> Nothing
                Left _ -> Nothing
              draftId_ = case contentObj .: "id" of
                Right d
                  | isNothing url_ -> Just d
                _ -> Nothing
              itemType_ =
                if isNothing url_ then "DRAFT_ISSUE"
                else "ISSUE"
            fieldValsJson <- lmap show $
              obj .: "fieldValues"
            case toObject fieldValsJson of
              Nothing -> Right Nothing
              Just fvObj -> do
                fvNodes :: Array Json <-
                  lmap show $ fvObj .: "nodes"
                let
                  status_ = extractFieldValue
                    "Status"
                    fvNodes
                  labels_ = extractLabels fvNodes
                Right $ Just $ ProjectItem
                  { itemId: itemId_
                  , draftId: draftId_
                  , title: title_
                  , status: status_
                  , itemType: itemType_
                  , url: url_
                  , repoName: repoName_
                  , labels: labels_
                  , number: number_
                  , body: body_
                  }

-- | Extract a single-select field value by name.
extractFieldValue
  :: String -> Array Json -> Maybe String
extractFieldValue fieldName nodes =
  Array.head $ catMaybes $ map
    ( \node -> case toObject node of
        Nothing -> Nothing
        Just obj ->
          case obj .: "field" of
            Right fieldJson ->
              case toObject fieldJson of
                Just fieldObj ->
                  case fieldObj .: "name" of
                    Right name
                      | name == fieldName ->
                          case obj .: "name" of
                            Right v -> Just v
                            Left _ -> Nothing
                    _ -> Nothing
                Nothing -> Nothing
            Left _ -> Nothing
    )
    nodes

-- | Extract labels from field value nodes.
extractLabels :: Array Json -> Array String
extractLabels nodes =
  catMaybes $ nodes >>= \node ->
    case toObject node of
      Nothing -> [ Nothing ]
      Just obj ->
        case obj .: "labels" of
          Right labelsJson ->
            case toObject labelsJson of
              Just labelsObj ->
                case labelsObj .: "nodes" of
                  Right
                    ( labelNodes
                        :: Array
                             { name :: String
                             }
                    ) ->
                    map (Just <<< _.name)
                      labelNodes
                  Left _ -> [ Nothing ]
              Nothing -> [ Nothing ]
          Left _ -> [ Nothing ]

-- | Parse the Status field metadata from project fields.
parseStatusField :: Json -> Maybe StatusField
parseStatusField json =
  case toObject json of
    Nothing -> Nothing
    Just obj ->
      case obj .: "fields" of
        Left _ -> Nothing
        Right fieldsJson ->
          case toObject fieldsJson of
            Nothing -> Nothing
            Just fObj ->
              case fObj .: "nodes" of
                Left _ -> Nothing
                Right (nodes :: Array Json) ->
                  Array.head $
                    mapMaybe parseOneField nodes
  where
  parseOneField :: Json -> Maybe StatusField
  parseOneField json = case toObject json of
    Nothing -> Nothing
    Just obj ->
      case obj .: "name", obj .: "id" of
        Right "Status", Right fid ->
          case obj .: "options" of
            Right
              ( opts
                  :: Array
                       { id :: String
                       , name :: String
                       }
              ) ->
              Just
                { fieldId: fid
                , options: map
                    ( \o ->
                        { optionId: o.id
                        , name: o.name
                        }
                    )
                    opts
                }
            Left _ -> Nothing
        _, _ -> Nothing

-- | Update the status of a project item.
updateItemStatus
  :: String
  -> String
  -> String
  -> String
  -> String
  -> Aff (Either String Unit)
updateItemStatus token projectId itemId fieldId optionId = do
  let
    mutation =
      "mutation { updateProjectV2ItemFieldValue("
        <> "input: {"
        <> " projectId: \""
        <> projectId
        <> "\""
        <> " itemId: \""
        <> itemId
        <> "\""
        <> " fieldId: \""
        <> fieldId
        <> "\""
        <> " value: { singleSelectOptionId: \""
        <> optionId
        <> "\" }"
        <> " }) { projectV2Item { id } } }"
  result <- ghGraphQL token mutation
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right unit

-- | Create a draft issue on a project board.
addDraftItem
  :: String
  -> String
  -> String
  -> Aff (Either String Unit)
addDraftItem token projectId title = do
  let
    mutation =
      "mutation { addProjectV2DraftIssue("
        <> "input: {"
        <> " projectId: \""
        <> projectId
        <> "\""
        <> " title: \""
        <> escapeGql title
        <> "\""
        <> " }) { projectItem { id } } }"
  result <- ghGraphQL token mutation
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right unit

-- | Update a draft issue title/body.
updateDraftItem
  :: String
  -> String
  -> String
  -> Aff (Either String Unit)
updateDraftItem token draftId title = do
  let
    mutation =
      "mutation { updateProjectV2DraftIssue("
        <> "input: {"
        <> " draftIssueId: \""
        <> draftId
        <> "\""
        <> " title: \""
        <> escapeGql title
        <> "\""
        <> " }) { draftIssue { id } } }"
  result <- ghGraphQL token mutation
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right unit

-- | Escape a string for use in a GraphQL query.
escapeGql :: String -> String
escapeGql = replaceAll (Pattern "\\") (Replacement "\\\\")
  >>> replaceAll (Pattern "\"") (Replacement "\\\"")
  >>> replaceAll (Pattern "\n") (Replacement "\\n")
