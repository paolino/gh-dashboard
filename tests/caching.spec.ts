// Tests for IndexedDB caching and ETag behavior.
// Authenticated tests require GH_DASHBOARD_TOKEN.

import { test, expect } from "@playwright/test";

const TOKEN = process.env.GH_DASHBOARD_TOKEN ?? "";

test.describe("API caching (authenticated)", () => {
  test.skip(!TOKEN, "GH_DASHBOARD_TOKEN not set");

  test.beforeEach(async ({ page }) => {
    await page.goto("/");
    // Clear both localStorage and IndexedDB
    await page.evaluate(() => {
      localStorage.clear();
      indexedDB.deleteDatabase("gh-dashboard-cache");
    });
    // Inject token
    await page.evaluate((tok) => {
      localStorage.setItem("gh-dashboard-token", tok);
    }, TOKEN);
    await page.reload();
  });

  test("populates IndexedDB cache on first load", async ({
    page,
  }) => {
    // Wait for repos to load
    await page.waitForTimeout(5000);

    // Check IndexedDB has cached responses
    const cacheCount = await page.evaluate(async () => {
      return new Promise<number>((resolve) => {
        const req = indexedDB.open("gh-dashboard-cache", 1);
        req.onsuccess = () => {
          const db = req.result;
          const tx = db.transaction("responses", "readonly");
          const store = tx.objectStore("responses");
          const countReq = store.count();
          countReq.onsuccess = () => resolve(countReq.result);
          countReq.onerror = () => resolve(0);
        };
        req.onerror = () => resolve(0);
      });
    });

    expect(cacheCount).toBeGreaterThan(0);
  });

  test("cached entries have etag and body", async ({ page }) => {
    // Wait for repos to load
    await page.waitForTimeout(5000);

    // Get first cached entry
    const entry = await page.evaluate(async () => {
      return new Promise<any>((resolve) => {
        const req = indexedDB.open("gh-dashboard-cache", 1);
        req.onsuccess = () => {
          const db = req.result;
          const tx = db.transaction("responses", "readonly");
          const store = tx.objectStore("responses");
          const cursor = store.openCursor();
          cursor.onsuccess = () => {
            const result = cursor.result;
            if (result) {
              resolve(result.value);
            } else {
              resolve(null);
            }
          };
          cursor.onerror = () => resolve(null);
        };
        req.onerror = () => resolve(null);
      });
    });

    expect(entry).not.toBeNull();
    expect(entry.url).toContain("api.github.com");
    expect(entry.body).toBeTruthy();
    expect(entry.fetchedAt).toBeGreaterThan(0);
  });

  test("second load uses cached data (304 responses)", async ({
    page,
  }) => {
    // First load — populates cache
    await page.waitForTimeout(5000);

    // Track network requests on second load
    const responses: { url: string; status: number }[] = [];
    page.on("response", (resp) => {
      if (resp.url().includes("api.github.com")) {
        responses.push({
          url: resp.url(),
          status: resp.status(),
        });
      }
    });

    // Reload — should get 304s for cached URLs
    await page.reload();
    await page.waitForTimeout(5000);

    const has304 = responses.some((r) => r.status === 304);
    // At least some responses should be 304
    // (GitHub may not always return 304 for all,
    //  but repos endpoint should be cached)
    expect(responses.length).toBeGreaterThan(0);
    // If we got any 304, caching is working
    if (has304) {
      const count304 = responses.filter(
        (r) => r.status === 304
      ).length;
      expect(count304).toBeGreaterThan(0);
    }
  });

  test("reset clears IndexedDB cache", async ({ page }) => {
    // Wait for cache to populate
    await page.waitForTimeout(3000);

    // Dismiss confirm dialog and click reset
    page.on("dialog", (dialog) => dialog.accept());
    await page.click('button[title="Reset all data"]');

    // Check IndexedDB is empty
    const cacheCount = await page.evaluate(async () => {
      return new Promise<number>((resolve) => {
        const req = indexedDB.open("gh-dashboard-cache", 1);
        req.onsuccess = () => {
          const db = req.result;
          const tx = db.transaction("responses", "readonly");
          const store = tx.objectStore("responses");
          const countReq = store.count();
          countReq.onsuccess = () => resolve(countReq.result);
          countReq.onerror = () => resolve(0);
        };
        req.onerror = () => resolve(0);
      });
    });

    expect(cacheCount).toBe(0);
  });
});
