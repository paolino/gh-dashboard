-- | IndexedDB-based API response cache.
-- |
-- | Provides async get/put for cached GitHub API
-- | responses. Each entry stores the URL, ETag,
-- | response body (JSON string), and fetch timestamp.
-- |
-- | Used by `GitHub.Rest.ghFetch` to:
-- | 1. Return cached data instantly on page load
-- | 2. Send `If-None-Match` with the stored ETag
-- | 3. Handle 304 Not Modified (return cached body)
-- | 4. Store fresh responses for next time
module FFI.Cache
  ( CachedResponse
  , getCachedResponse
  , putCachedResponse
  , clearCache
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect (Effect)
import Effect.Exception (Error)
import Data.Either (Either(..))

-- | A cached API response from IndexedDB.
type CachedResponse =
  { url :: String
  , etag :: String
  , body :: String
  , fetchedAt :: Number
  }

foreign import getCachedResponseImpl
  :: String
  -> (CachedResponse -> Effect Unit)
  -> Effect Unit
  -> Effect Unit

foreign import putCachedResponseImpl
  :: String
  -> String
  -> String
  -> Effect Unit
  -> Effect Unit

foreign import clearCacheImpl
  :: Effect Unit -> Effect Unit

-- | Look up a cached response by URL.
-- | Returns Nothing if not cached or on error.
getCachedResponse :: String -> Aff (Maybe CachedResponse)
getCachedResponse url = makeAff \cb -> do
  getCachedResponseImpl url
    (\r -> cb (Right (Just r)))
    (cb (Right Nothing))
  pure nonCanceler

-- | Store a response in the cache.
putCachedResponse
  :: String -> String -> String -> Aff Unit
putCachedResponse url etag body = makeAff \cb -> do
  putCachedResponseImpl url etag body
    (cb (Right unit))
  pure nonCanceler

-- | Clear all cached responses.
clearCache :: Aff Unit
clearCache = makeAff \cb -> do
  clearCacheImpl (cb (Right unit))
  pure nonCanceler
