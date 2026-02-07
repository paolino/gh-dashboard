-- | FFI to toggle light/dark theme class on the body element.
module FFI.Theme (setBodyTheme) where

import Prelude

import Effect (Effect)

-- | Set body theme. `true` = dark (default), `false` = light.
foreign import setBodyTheme :: Boolean -> Effect Unit
