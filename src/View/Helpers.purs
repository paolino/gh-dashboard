-- | Shared rendering helpers used across view sub-modules.
module View.Helpers
  ( renderMarkdownRow
  , linkButton
  , detailHead
  , renderAssignees
  , renderLabels
  , formatDate
  , parseMarkdownImpl
  ) where

import Prelude

import Data.Array (null)
import Data.Maybe (Maybe(..))
import Data.String (joinWith, take)
import Halogen.HTML as HH
import Halogen.HTML.Core (PropName(..))
import Halogen.HTML.Properties as HP
import Types (Assignee, Label)

foreign import parseMarkdownImpl :: String -> String

-- | Row with markdown-rendered body.
renderMarkdownRow
  :: forall w i
   . Maybe String
  -> Array (HH.HTML w i)
renderMarkdownRow = case _ of
  Nothing -> []
  Just "" -> []
  Just body ->
    [ HH.tr
        [ HP.class_ (HH.ClassName "detail-row") ]
        [ HH.td
            [ HP.colSpan 6 ]
            [ HH.div
                [ HP.class_
                    (HH.ClassName "detail-body")
                , HP.prop
                    ( PropName "innerHTML"
                        :: PropName String
                    )
                    (parseMarkdownImpl body)
                ]
                []
            ]
        ]
    ]

-- | Small link button that opens a URL.
linkButton :: forall w i. String -> HH.HTML w i
linkButton url =
  HH.a
    [ HP.href url
    , HP.target "_blank"
    , HP.class_ (HH.ClassName "link-btn")
    ]
    [ HH.text "\x2197" ]

-- | Column headers for detail tables.
detailHead :: forall w i. HH.HTML w i
detailHead =
  HH.thead_
    [ HH.tr_
        [ HH.th_ [ HH.text "Title" ]
        , HH.th_ [ HH.text "Assignees" ]
        , HH.th_ [ HH.text "Author" ]
        , HH.th_ [ HH.text "Date" ]
        , HH.th_ []
        , HH.th_ []
        ]
    ]

-- | Render assignees.
renderAssignees
  :: forall w i. Array Assignee -> HH.HTML w i
renderAssignees assignees =
  if null assignees then HH.text ""
  else
    HH.span
      [ HP.class_
          (HH.ClassName "detail-assignees")
      ]
      [ HH.text
          (joinWith ", " (map _.login assignees))
      ]

-- | Render label tags.
renderLabels
  :: forall w i. Array Label -> HH.HTML w i
renderLabels labels =
  if null labels then HH.text ""
  else
    HH.span
      [ HP.class_
          (HH.ClassName "detail-labels")
      ]
      ( map
          ( \l ->
              HH.span
                [ HP.class_
                    (HH.ClassName "label-tag")
                ]
                [ HH.text l.name ]
          )
          labels
      )

-- | Format ISO date to short form (YYYY-MM-DD).
formatDate :: String -> String
formatDate = take 10
