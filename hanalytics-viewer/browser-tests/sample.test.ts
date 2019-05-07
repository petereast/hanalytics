// import { expect } from 'chai';
import { baseUrl, chrome } from "./help";

describe("Browser test sample", async () => {
  it("Loads the auth page", async () => {
    const { browser, page } = await chrome();

    await page.goto(`${baseUrl}/auth`);

    await page.$(".auth-page");

    await browser.close();
  });
});
