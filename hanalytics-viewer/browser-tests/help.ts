import * as puppeteer from "puppeteer";
import { Page, Browser } from "puppeteer";

const port = process.env.PORT || 7175;
const headless = process.env.HEADLESS === "true" || !!process.env.CI || false;

export const baseUrl = `http://localhost:${port}`;

export async function chrome(): Promise<{ browser: Browser; page: Page }> {
  const browser = await puppeteer.launch({
    headless,
    ignoreHTTPSErrors: true,
    defaultViewport: {
      width: 1366,
      height: 768,
    },
    args: ["--no-sandbox"],
  });

  const page = await browser.newPage();

  return { browser, page };
}
