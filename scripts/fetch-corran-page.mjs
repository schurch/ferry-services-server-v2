import { chromium } from "playwright";

const url = process.argv[2];

if (!url) {
  console.error("Usage: node fetch-corran-page.mjs <url>");
  process.exit(1);
}

const browser = await chromium.launch({ headless: true });

try {
  const page = await browser.newPage({
    userAgent:
      "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36",
  });

  await page.goto(url, {
    waitUntil: "domcontentloaded",
    timeout: 30000,
  });

  await page.waitForSelector("a.listing__link", { timeout: 15000 });
  process.stdout.write(await page.content());
} finally {
  await browser.close();
}
