-- | FFI for browser dialog boxes.
module FFI.Dialog (confirmDialog) where

import Effect (Effect)

-- | Show a confirm dialog, returning true if accepted.
foreign import confirmDialog :: String -> Effect Boolean
