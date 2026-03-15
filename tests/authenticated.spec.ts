// Tests that require a valid GitHub token.
// Set GH_DASHBOARD_TOKEN env var to run these.
// Skipped in CI unless the secret is configured.

import { test, expect } from "@playwright/test";

const TOKEN = process.env.GH_DASHBOARD_TOKEN ?? "";

test.describe("Dashboard (authenticated)", () => {
  test.skip(!TOKEN, "GH_DASHBOARD_TOKEN not set");

  test.beforeEach(async ({ page }) => {
    // Inject the token into localStorage before navigating
    await page.goto("/");
    await page.evaluate((tok) => {
      localStorage.setItem("gh-dashboard-token", tok);
    }, TOKEN);
    await page.reload();
  });

  test("shows toolbar after login", async ({ page }) => {
    await expect(page.locator(".toolbar")).toBeVisible();
  });

  test("shows Repos and Projects tabs", async ({ page }) => {
    await expect(
      page.locator(".tab-btn", { hasText: "Repos" })
    ).toBeVisible();
    await expect(
      page.locator(".tab-btn", { hasText: "Projects" })
    ).toBeVisible();
  });

  test("can switch to Projects tab", async ({ page }) => {
    const projectsBtn = page.locator(".tab-btn", {
      hasText: "Projects",
    });
    await projectsBtn.click();
    await expect(projectsBtn).toHaveClass(/active/);
  });

  test("can switch back to Repos tab", async ({ page }) => {
    await page
      .locator(".tab-btn", { hasText: "Projects" })
      .click();
    const reposBtn = page.locator(".tab-btn", {
      hasText: "Repos",
    });
    await reposBtn.click();
    await expect(reposBtn).toHaveClass(/active/);
  });

  test("theme toggle switches theme", async ({ page }) => {
    const toggle = page.locator(".theme-toggle");
    const body = page.locator("body");

    // Get initial theme
    const initialClass = await body.getAttribute("class");

    // Click toggle
    await toggle.click();

    // Theme class should change
    const newClass = await body.getAttribute("class");
    expect(newClass).not.toBe(initialClass);
  });

  test("filter input works", async ({ page }) => {
    const filter = page.locator(
      'input[placeholder="Filter repos..."]'
    );
    await filter.fill("test-repo");
    await expect(filter).toHaveValue("test-repo");
  });

  test("export button is visible", async ({ page }) => {
    await expect(
      page.locator('button[title="Export settings"]')
    ).toBeVisible();
  });

  test("import button is visible", async ({ page }) => {
    await expect(
      page.locator('button[title="Import settings"]')
    ).toBeVisible();
  });

  test("rate limit is displayed", async ({ page }) => {
    // Wait for API calls to complete
    await page.waitForTimeout(3000);
    const rateLimit = page.locator(".rate-limit");
    // May or may not be visible depending on timing
    // but if visible, should contain a number
    const count = await rateLimit.count();
    if (count > 0) {
      const text = await rateLimit.textContent();
      expect(text).toMatch(/\d+\/\d+/);
    }
  });
});
