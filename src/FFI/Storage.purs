-- | FFI for exporting/importing dashboard settings and
-- | encrypted token storage.
module FFI.Storage
  ( exportStorage
  , importStorage
  , saveTokenEncrypted
  , loadTokenEncrypted
  ) where

import Prelude

import Effect (Effect)
import Promise (Promise)

-- | Download all settings (including passphrase-encrypted
-- | token) as a JSON file.
foreign import exportStorage :: Effect Unit

-- | Open a file picker, parse the JSON, restore settings
-- | (decrypting token with passphrase), reload.
foreign import importStorage :: Effect Unit

-- | Encrypt a token with a random local key and store it.
foreign import saveTokenEncrypted
  :: String -> Effect (Promise Unit)

-- | Load and decrypt the token from localStorage.
-- | Returns empty string if no token is stored.
-- | Handles migration from plaintext tokens.
foreign import loadTokenEncrypted
  :: Effect (Promise String)
