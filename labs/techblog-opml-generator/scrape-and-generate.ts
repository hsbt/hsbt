/**
 * Techblog OPML Generator with Puppeteer
 * 
 * This script automates the scraping process from https://hatena.blog/dev/companies
 * and generates an OPML file for RSS readers.
 * 
 * Requirements:
 * - Node.js
 * - npm packages: puppeteer, fs
 */

import * as puppeteer from 'puppeteer';
import * as fs from 'fs';

/**
 * Escape XML special characters
 */
function escapeXml(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&apos;');
}

/**
 * Generate OPML XML from blog information
 */
function generateOPML(blogs: [string, string][]): string {
  const outlines = blogs.map(([title, url]) => 
    `    <outline text="${escapeXml(title)}" title="${escapeXml(title)}" type="rss" xmlUrl="${escapeXml(url)}"/>`
  ).join('\n');
  
  return `<?xml version="1.0" encoding="UTF-8"?>
<opml version="1.0">
  <head>
    <title>企業テックブログOPML</title>
  </head>
  <body>
${outlines}
  </body>
</opml>`;
}

/**
 * Main function to scrape the page and generate OPML
 */
async function main() {
  console.log('Launching browser...');
  const browser = await puppeteer.launch({
    headless: false, // Set to true for headless mode
    defaultViewport: { width: 1200, height: 800 }
  });

  try {
    const page = await browser.newPage();
    console.log('Navigating to Hatena Blog companies page...');
    await page.goto('https://hatena.blog/dev/companies', { waitUntil: 'networkidle2' });
    
    console.log('Scrolling to load all content...');
    // Scroll to bottom until no more content is loaded
    let previousHeight;
    let currentHeight = 0;
    while (previousHeight !== currentHeight) {
      previousHeight = currentHeight;
      currentHeight = await page.evaluate(() => {
        window.scrollTo(0, document.body.scrollHeight);
        return document.body.scrollHeight;
      });
      await page.waitForTimeout(2000); // Wait for 2 seconds after scrolling
    }
    
    console.log('Extracting blog information...');
    // Extract blog information
    const blogs = await page.evaluate(() => {
      const results: [string, string][] = [];
      document.querySelectorAll('ul > li > div > div').forEach(element => {
        try {
          const titleElement = element.querySelector('a > h4');
          const linkElement = element.querySelector('a');
          
          if (titleElement && linkElement) {
            const title = titleElement.textContent?.trim() || '';
            const url = linkElement.getAttribute('href') || '';
            
            if (title && url) {
              // Assuming we need to add '/feed' to the URL to get the RSS feed
              const feedUrl = url.endsWith('/') ? `${url}feed` : `${url}/feed`;
              results.push([title, feedUrl]);
            }
          }
        } catch (error) {
          console.error('Error processing element:', error);
        }
      });
      return results;
    });
    
    console.log(`Found ${blogs.length} blogs`);
    
    // Generate OPML
    const opml = generateOPML(blogs);
    
    // Save to file
    const outputFile = 'hatena-tech-blogs.opml';
    fs.writeFileSync(outputFile, opml);
    console.log(`OPML file saved to ${outputFile}`);

    // Also output the raw blog information for reference
    const blogsOutput = blogs.map(([title, url]) => `["${title}", "${url}"]`).join(',\n  ');
    const arrayOutput = `[\n  ${blogsOutput}\n]`;
    fs.writeFileSync('tech-blogs-list.ts', `export const techBlogs = ${arrayOutput};\n`);
    console.log('Blog list saved to tech-blogs-list.ts');
    
  } catch (error) {
    console.error('Error:', error);
  } finally {
    await browser.close();
  }
}

main();