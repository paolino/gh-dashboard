// Tests that run without a GitHub token.
// These verify the login page UI shell works correctly.

import { test, expect } from "@playwright/test";

test.describe("Token form (unauthenticated)", () => {
  test.beforeEach(async ({ page }) => {
    // Clear any saved token so we always see the login form
    await page.goto("/");
    await page.evaluate(() => localStorage.clear());
    await page.reload();
  });

  test("shows login form on first visit", async ({ page }) => {
    await expect(page.locator("h1")).toHaveText("GH Dashboard");
    await expect(
      page.locator('input[type="password"]')
    ).toBeVisible();
    await expect(page.locator("button.btn", { hasText: "Connect" })).toBeVisible();
  });

  test("shows getting started instructions", async ({ page }) => {
    await expect(page.locator("text=Getting started")).toBeVisible();
    await expect(
      page.locator("text=Create a GitHub token")
    ).toBeVisible();
  });

  test("shows error on empty token submit", async ({ page }) => {
    await page.click("text=Connect");
    await expect(page.locator(".error")).toHaveText(
      "Please enter a token"
    );
  });

  test("token input accepts text", async ({ page }) => {
    const input = page.locator('input[type="password"]');
    await input.fill("ghp_test123");
    await expect(input).toHaveValue("ghp_test123");
  });
});
