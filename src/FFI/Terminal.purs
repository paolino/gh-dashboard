-- | FFI to embed xterm.js terminals inline.
module FFI.Terminal
  ( attachTerminal
  , destroyTerminal
  ) where

import Prelude

import Effect (Effect)

-- | Create an xterm.js terminal in the given DOM element
-- | and connect it via WebSocket.
foreign import attachTerminal
  :: String -> String -> Effect Unit

-- | Destroy a terminal instance by element ID.
foreign import destroyTerminal
  :: String -> Effect Unit
