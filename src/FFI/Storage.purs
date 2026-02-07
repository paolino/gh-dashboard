-- | FFI for exporting and importing dashboard settings.
module FFI.Storage
  ( exportStorage
  , importStorage
  ) where

import Prelude

import Effect (Effect)

-- | Download all non-token settings as a JSON file.
foreign import exportStorage :: Effect Unit

-- | Open a file picker, parse the JSON, restore settings, reload.
foreign import importStorage :: Effect Unit
