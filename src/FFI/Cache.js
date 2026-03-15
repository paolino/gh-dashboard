// IndexedDB-based API response cache.
//
// Schema: object store "responses" keyed by URL, storing:
//   { url, etag, body (JSON string), fetchedAt (epoch ms) }
//
// The DB is created lazily on first access and reused.
// All operations are best-effort — errors resolve with
// fallback values rather than rejecting.

let dbPromise = null;

function openDB() {
  if (dbPromise) return dbPromise;
  dbPromise = new Promise((resolve, reject) => {
    const req = indexedDB.open("gh-dashboard-cache", 1);
    req.onupgradeneeded = () => {
      const db = req.result;
      if (!db.objectStoreNames.contains("responses")) {
        db.createObjectStore("responses", { keyPath: "url" });
      }
    };
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });
  return dbPromise;
}

// getCachedResponseImpl :: String -> (CachedResponse -> Effect Unit) -> Effect Unit -> Effect Unit
export function getCachedResponseImpl(url) {
  return function (onFound) {
    return function (onMissing) {
      return function () {
        openDB()
          .then((db) => {
            const tx = db.transaction("responses", "readonly");
            const store = tx.objectStore("responses");
            const req = store.get(url);
            req.onsuccess = () => {
              if (req.result) {
                onFound(req.result)();
              } else {
                onMissing();
              }
            };
            req.onerror = () => onMissing();
          })
          .catch(() => onMissing());
      };
    };
  };
}

// putCachedResponseImpl :: String -> String -> String -> Effect Unit -> Effect Unit
export function putCachedResponseImpl(url) {
  return function (etag) {
    return function (body) {
      return function (onDone) {
        return function () {
          openDB()
            .then((db) => {
              const tx = db.transaction("responses", "readwrite");
              const store = tx.objectStore("responses");
              store.put({
                url: url,
                etag: etag,
                body: body,
                fetchedAt: Date.now(),
              });
              tx.oncomplete = () => onDone();
              tx.onerror = () => onDone();
            })
            .catch(() => onDone());
        };
      };
    };
  };
}

// clearCacheImpl :: Effect Unit -> Effect Unit
export function clearCacheImpl(onDone) {
  return function () {
    openDB()
      .then((db) => {
        const tx = db.transaction("responses", "readwrite");
        const store = tx.objectStore("responses");
        store.clear();
        tx.oncomplete = () => onDone();
        tx.onerror = () => onDone();
      })
      .catch(() => onDone());
  };
}
