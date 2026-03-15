-- | GitHub GraphQL API client for Projects v2.
-- |
-- | This module handles all interaction with the GitHub
-- | GraphQL endpoint (`api.github.com/graphql`):
-- |
-- | **Queries:**
-- | - `fetchUserProjects` — list the user's Projects v2
-- |   boards (light: metadata only, no items).
-- | - `fetchProjectItems` — paginated fetch of all items
-- |   in a single project, including status field
-- |   metadata for drag-and-drop status changes.
-- |
-- | **Mutations:**
-- | - `updateItemStatus` — move an item to a new status
-- |   column.
-- | - `addDraftItem` / `updateDraftItem` — create and
-- |   edit draft issues on a board.
-- | - `deleteProjectItem` — remove an item.
-- | - `renameProject` — change a project's title.
-- |
-- | The GraphQL response navigation (`navigateProjects`,
-- | `navigateProjectItems`) is intentionally verbose to
-- | give clear error messages when the response shape
-- | changes.
module GitHub.GraphQL
  ( ghGraphQL
  , fetchUserProjects
  , fetchProjectItems
  , updateItemStatus
  , addDraftItem
  , updateDraftItem
  , deleteProjectItem
  , renameProject
  ) where

import Prelude

import Data.Argonaut.Core
  ( Json
  , jsonEmptyObject
  , stringify
  , toObject
  )
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (catMaybes, head, mapMaybe)
import Data.Array (head) as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Array (intercalate)
import Data.Traversable (traverse)
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff, try)
import Effect.Exception (message)
import Fetch (fetch)
import Types
  ( Project(..)
  , ProjectItem(..)
  , StatusField
  )

------------------------------------------------------------
-- Core GraphQL transport
------------------------------------------------------------

-- | POST a GraphQL query to the GitHub API.
-- |
-- | Sends `{ query, variables }` as JSON and returns
-- | the parsed response. GraphQL-level errors (the
-- | `errors` array) are extracted and surfaced as
-- | `Left`.
ghGraphQL
  :: String
  -> String
  -> Json
  -> Aff (Either String Json)
ghGraphQL token query variables = do
  result <- try do
    let
      body = encodeJson
        ( "query" := query
            ~> "variables" := variables
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
      Right json -> case extractGraphQLErrors json of
        Just err -> pure $ Left err
        Nothing -> pure $ Right json

-- | Extract error messages from a GraphQL response.
-- | Returns Nothing if no errors field or it's empty.
extractGraphQLErrors :: Json -> Maybe String
extractGraphQLErrors json = do
  obj <- toObject json
  case obj .: "errors" of
    Left _ -> Nothing
    Right (errArr :: Array Json) ->
      let
        msgs = mapMaybe extractMessage errArr
      in
        if msgs == [] then
          Just "GraphQL error (no details)"
        else Just (intercalate "; " msgs)
  where
  extractMessage :: Json -> Maybe String
  extractMessage j = do
    o <- toObject j
    case o .: "message" of
      Left _ -> Nothing
      Right (m :: String) -> Just m

------------------------------------------------------------
-- GraphQL queries (raw strings)
------------------------------------------------------------

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
projectItemsQuery :: String
projectItemsQuery =
  """
  query($nodeId: ID!, $cursor: String) {
    node(id: $nodeId) {
      ... on ProjectV2 {
        fields(first: 20) { nodes {
          ... on ProjectV2SingleSelectField {
            id name options { id name }
          }
        } }
        items(first: 100, after: $cursor) {
          pageInfo { hasNextPage endCursor }
          nodes { id fieldValues(first: 10) { nodes {
            ... on ProjectV2ItemFieldSingleSelectValue {
              name field {
                ... on ProjectV2SingleSelectField { name }
              }
            }
            ... on ProjectV2ItemFieldLabelValue {
              labels(first: 10) { nodes { name } }
              field {
                ... on ProjectV2FieldCommon { name }
              }
            }
          } }
          content {
            ... on Issue { title url number body
              repository { nameWithOwner } }
            ... on PullRequest { title url number body
              repository { nameWithOwner } }
            ... on DraftIssue { id title body }
          } }
        }
      }
    }
  }
  """

------------------------------------------------------------
-- Queries
------------------------------------------------------------

-- | Fetch the authenticated user's Projects v2
-- | (light: no items, just metadata).
fetchUserProjects
  :: String
  -> Aff (Either String (Array Project))
fetchUserProjects token = do
  result <- ghGraphQL token projectsListQuery
    jsonEmptyObject
  case result of
    Left err -> pure $ Left err
    Right json ->
      pure $ navigateProjects json

-- | Fetch items for a single project (paginated).
-- |
-- | Returns both the items and the Status field
-- | metadata (field ID + option IDs) needed for
-- | drag-and-drop status changes.
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
    let
      vars = encodeJson
        ( "nodeId" := projectId
            ~> "cursor" := mCursor
            ~> jsonEmptyObject
        )
    result <- ghGraphQL token
      projectItemsQuery
      vars
    case result of
      Left err -> pure $ Left err
      Right json ->
        case navigateProjectItems json of
          Left err -> pure $ Left err
          Right page ->
            let
              all = acc <> page.items
            in
              if page.hasNextPage then
                fetchPage page.endCursor all
              else
                pure $ Right
                  { items: all
                  , statusField: page.statusField
                  }

------------------------------------------------------------
-- Mutations
------------------------------------------------------------

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
    vars = encodeJson
      ( "projectId" := projectId
          ~> "itemId" := itemId
          ~> "fieldId" := fieldId
          ~> "optionId" := optionId
          ~> jsonEmptyObject
      )
  result <- ghGraphQL token
    """
    mutation(
      $projectId: ID!
      $itemId: ID!
      $fieldId: ID!
      $optionId: String!
    ) {
      updateProjectV2ItemFieldValue(input: {
        projectId: $projectId
        itemId: $itemId
        fieldId: $fieldId
        value: { singleSelectOptionId: $optionId }
      }) { projectV2Item { id } }
    }
    """
    vars
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
    vars = encodeJson
      ( "projectId" := projectId
          ~> "title" := title
          ~> jsonEmptyObject
      )
  result <- ghGraphQL token
    """
    mutation($projectId: ID!, $title: String!) {
      addProjectV2DraftIssue(input: {
        projectId: $projectId
        title: $title
      }) { projectItem { id } }
    }
    """
    vars
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
    vars = encodeJson
      ( "draftIssueId" := draftId
          ~> "title" := title
          ~> jsonEmptyObject
      )
  result <- ghGraphQL token
    """
    mutation($draftIssueId: ID!, $title: String!) {
      updateProjectV2DraftIssue(input: {
        draftIssueId: $draftIssueId
        title: $title
      }) { draftIssue { id } }
    }
    """
    vars
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right unit

-- | Rename a project.
renameProject
  :: String
  -> String
  -> String
  -> Aff (Either String Unit)
renameProject token projectId title = do
  let
    vars = encodeJson
      ( "projectId" := projectId
          ~> "title" := title
          ~> jsonEmptyObject
      )
  result <- ghGraphQL token
    """
    mutation($projectId: ID!, $title: String!) {
      updateProjectV2(input: {
        projectId: $projectId
        title: $title
      }) { projectV2 { id } }
    }
    """
    vars
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right unit

-- | Delete a project item.
deleteProjectItem
  :: String
  -> String
  -> String
  -> Aff (Either String Unit)
deleteProjectItem token projectId itemId = do
  let
    vars = encodeJson
      ( "projectId" := projectId
          ~> "itemId" := itemId
          ~> jsonEmptyObject
      )
  result <- ghGraphQL token
    """
    mutation($projectId: ID!, $itemId: ID!) {
      deleteProjectV2Item(input: {
        projectId: $projectId
        itemId: $itemId
      }) { deletedItemId }
    }
    """
    vars
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right unit

------------------------------------------------------------
-- Response navigation helpers
------------------------------------------------------------

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

-- | Navigate GraphQL response to one page of
-- | project items.
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
    endCursor_ =
      case
        pageInfoObj .: "endCursor"
        of
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
              number_ =
                case
                  contentObj .: "number"
                  of
                  Right n -> Just n
                  Left _ -> Nothing
              body_ = case contentObj .: "body" of
                Right b -> Just b
                Left _ -> Nothing
              repoName_ =
                case
                  contentObj .: "repository"
                  of
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
  head $ catMaybes $ map
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

-- | Parse the Status field metadata from
-- | project fields.
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
  parseOneField fJson = case toObject fJson of
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
