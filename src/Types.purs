-- | Domain types and JSON decode instances for the GitHub dashboard.
module Types
  ( Repo(..)
  , Issue(..)
  , PullRequest(..)
  , RepoDetail
  , Label
  , Assignee
  ) where

import Prelude

import Data.Argonaut.Core (toObject)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | A label on an issue or PR.
type Label =
  { name :: String
  }

-- | An assignee on an issue or PR.
type Assignee =
  { login :: String
  }

-- | A GitHub repository owned by the authenticated user.
newtype Repo = Repo
  { id :: Number
  , name :: String
  , fullName :: String
  , htmlUrl :: String
  , description :: Maybe String
  , language :: Maybe String
  , visibility :: String
  , openIssuesCount :: Int
  , updatedAt :: String
  , ownerLogin :: String
  }

instance DecodeJson Repo where
  decodeJson json = case toObject json of
    Nothing -> Left (TypeMismatch "Object")
    Just obj -> do
      id_ <- obj .: "id"
      name_ <- obj .: "name"
      fullName_ <- obj .: "full_name"
      htmlUrl_ <- obj .: "html_url"
      description_ <- obj .:? "description"
      language_ <- obj .:? "language"
      visibility_ <- obj .: "visibility"
      openIssuesCount_ <- obj .: "open_issues_count"
      updatedAt_ <- obj .: "updated_at"
      ownerObj <- obj .: "owner"
      ownerLogin_ <- ownerObj .: "login"
      pure $ Repo
        { id: id_
        , name: name_
        , fullName: fullName_
        , htmlUrl: htmlUrl_
        , description: description_
        , language: language_
        , visibility: visibility_
        , openIssuesCount: openIssuesCount_
        , updatedAt: updatedAt_
        , ownerLogin: ownerLogin_
        }

-- | An open issue (excluding pull requests).
newtype Issue = Issue
  { number :: Int
  , title :: String
  , htmlUrl :: String
  , createdAt :: String
  , userLogin :: String
  , labels :: Array Label
  , assignees :: Array Assignee
  , body :: Maybe String
  }

instance DecodeJson Issue where
  decodeJson json = case toObject json of
    Nothing -> Left (TypeMismatch "Object")
    Just obj -> do
      number_ <- obj .: "number"
      title_ <- obj .: "title"
      htmlUrl_ <- obj .: "html_url"
      createdAt_ <- obj .: "created_at"
      userObj <- obj .: "user"
      userLogin_ <- userObj .: "login"
      labels_ <- obj .: "labels"
      assignees_ <- obj .: "assignees"
      body_ <- obj .:? "body"
      pure $ Issue
        { number: number_
        , title: title_
        , htmlUrl: htmlUrl_
        , createdAt: createdAt_
        , userLogin: userLogin_
        , labels: labels_
        , assignees: assignees_
        , body: body_
        }

-- | An open pull request.
newtype PullRequest = PullRequest
  { number :: Int
  , title :: String
  , htmlUrl :: String
  , createdAt :: String
  , userLogin :: String
  , draft :: Boolean
  , labels :: Array Label
  , assignees :: Array Assignee
  , body :: Maybe String
  }

instance DecodeJson PullRequest where
  decodeJson json = case toObject json of
    Nothing -> Left (TypeMismatch "Object")
    Just obj -> do
      number_ <- obj .: "number"
      title_ <- obj .: "title"
      htmlUrl_ <- obj .: "html_url"
      createdAt_ <- obj .: "created_at"
      userObj <- obj .: "user"
      userLogin_ <- userObj .: "login"
      draft_ <- obj .: "draft"
      labels_ <- obj .: "labels"
      assignees_ <- obj .: "assignees"
      body_ <- obj .:? "body"
      pure $ PullRequest
        { number: number_
        , title: title_
        , htmlUrl: htmlUrl_
        , createdAt: createdAt_
        , userLogin: userLogin_
        , draft: draft_
        , labels: labels_
        , assignees: assignees_
        , body: body_
        }

-- | Cached detail for an expanded repo.
type RepoDetail =
  { issues :: Array Issue
  , pullRequests :: Array PullRequest
  , issueCount :: Int
  , prCount :: Int
  }
