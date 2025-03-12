/**
 * Techblog OPML Generator
 * 
 * This script scrapes tech blog information from https://hatena.blog/dev/companies
 * and generates an OPML file for RSS readers.
 * 
 * This script can be run in two modes:
 * 1. Browser console mode: Execute directly in browser after loading the page
 * 2. Node.js mode: Automated scraping using Puppeteer
 */

import * as puppeteer from 'puppeteer';
import * as fs from 'fs';
import axios from 'axios';

// The script is designed to be executed in browser console after scrolling to the bottom of the page
// or automatically using Puppeteer in Node.js environment

/**
 * Extract blog information from the hatena.blog/dev/companies page
 * @returns Array of [title, url] pairs
 */
function extractBlogInfo(): [string, string][] {
  const blogs: [string, string][] = [];
  
  // Get all company blog elements
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
          blogs.push([title, feedUrl]);
        }
      }
    } catch (error) {
      console.error('Error processing element:', error);
    }
  });
  
  return blogs;
}

/**
 * Interface for feed information from tech-blog-rss-feed
 */
interface FeedInfo {
  sourceName: string;
  feedUrl: string;
}

/**
 * Fetch feed information from GitHub repository
 * @returns Promise<FeedInfo[]>
 */
async function fetchGitHubFeedInfo(): Promise<FeedInfo[]> {
  try {
    const response = await axios.get('https://raw.githubusercontent.com/yamadashy/tech-blog-rss-feed/main/src/resources/feed-info-list.ts');
    const content = response.data as string;

    const feedEntries: FeedInfo[] = [];
    // 配列エントリーを抽出する正規表現
    const regex = /\['([^']+)',\s*'([^']+)'\]/g;
    let match;

    while ((match = regex.exec(content)) !== null) {
      const feedInfo: FeedInfo = {
        sourceName: match[1],
        feedUrl: match[2]
      };
      feedEntries.push(feedInfo);
    }

    console.log(`Successfully parsed ${feedEntries.length} feeds from GitHub`);
    return feedEntries;
  } catch (error) {
    console.error('Error fetching GitHub feed info:', error);
    return [];
  }
}

/**
 * Generate OPML XML from blog information including GitHub feeds
 * @param blogs Array of [title, url] pairs from Hatena
 * @param githubFeeds Array of FeedInfo from GitHub
 * @returns OPML XML string
 */
function generateOPML(blogs: [string, string][], githubFeeds: FeedInfo[] = []): string {
  // URLの重複を管理するためのSet
  const uniqueUrls = new Set<string>();
  const duplicateUrls = new Set<string>();
  
  // Hatenaブログの重複チェックと追加
  const hatenaOutlines = blogs.filter(([_, url]) => {
    if (uniqueUrls.has(url)) {
      duplicateUrls.add(url);
      return false;
    }
    uniqueUrls.add(url);
    return true;
  }).map(([title, url]) => 
    `    <outline text="${escapeXml(title)}" title="${escapeXml(title)}" type="rss" xmlUrl="${escapeXml(url)}"/>`
  );
  
  // GitHubフィードの重複チェックと追加
  const githubOutlines = githubFeeds.filter(feed => {
    if (uniqueUrls.has(feed.feedUrl)) {
      duplicateUrls.add(feed.feedUrl);
      return false;
    }
    uniqueUrls.add(feed.feedUrl);
    return true;
  }).map(feed => 
    `    <outline text="${escapeXml(feed.sourceName)}" title="${escapeXml(feed.sourceName)}" type="rss" xmlUrl="${escapeXml(feed.feedUrl)}"/>`
  );
  
  console.log(`Removed ${duplicateUrls.size} duplicate URLs`);
  
  const allOutlines = [...hatenaOutlines, ...githubOutlines].join('\n');
  
  return `<?xml version="1.0" encoding="UTF-8"?>
<opml version="1.0">
  <head>
    <title>企業テックブログOPML</title>
  </head>
  <body>
${allOutlines}
  </body>
</opml>`;
}

/**
 * Escape XML special characters
 * @param text Text to escape
 * @returns Escaped text
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
 * Automated scraping using Puppeteer
 */
async function scrapeWithPuppeteer() {
  console.log('Launching browser...');
  const browser = await puppeteer.launch({
    headless: false,
    defaultViewport: { width: 1200, height: 800 }
  });

  try {
    const page = await browser.newPage();
    console.log('Navigating to Hatena Blog companies page...');
    await page.goto('https://hatena.blog/dev/companies', { waitUntil: 'networkidle2' });
    
    console.log('Scrolling to load all content...');
    let previousHeight;
    let currentHeight = 0;
    while (previousHeight !== currentHeight) {
      previousHeight = currentHeight;
      currentHeight = await page.evaluate(() => {
        window.scrollTo(0, document.body.scrollHeight);
        return document.body.scrollHeight;
      });
      await page.waitForTimeout(2000);
    }
    
    console.log('Extracting blog information...');
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
              const feedUrl = url.endsWith('/') ? `${url}feed` : `${url}/feed`;
              results.push([title, feedUrl]);
            }
          }
        } catch (error: unknown) {
          console.error('Error processing element:', error);
        }
      });
      return results;
    });
    
    console.log(`Found ${blogs.length} blogs from Hatena`);
    
    console.log('Fetching additional feeds from GitHub...');
    const githubFeeds = await fetchGitHubFeedInfo();
    console.log(`Found ${githubFeeds.length} blogs from GitHub`);
    
    const opml = generateOPML(blogs, githubFeeds);
    const outputFile = 'tech-blogs.opml';
    fs.writeFileSync(outputFile, opml);
    console.log(`OPML file saved to ${outputFile}`);
    
  } catch (error) {
    console.error('Error:', error);
  } finally {
    await browser.close();
  }
}

/**
 * Main function that can be run in both browser and Node.js environments
 */
async function main() {
  if (typeof window === 'undefined') {
    // Node.js environment - use Puppeteer
    await scrapeWithPuppeteer();
  } else {
    // Browser environment - use direct DOM manipulation
    console.log("Extracting blog information...");
    const blogs = extractBlogInfo();
    console.log(`Found ${blogs.length} blogs from Hatena`);
    
    console.log("Fetching additional feeds from GitHub...");
    const githubFeeds = await fetchGitHubFeedInfo();
    console.log(`Found ${githubFeeds.length} blogs from GitHub`);
    
    const opml = generateOPML(blogs, githubFeeds);
    console.log("OPML generated:");
    console.log(opml);
    
    if (typeof navigator !== 'undefined' && navigator.clipboard) {
      navigator.clipboard.writeText(opml)
        .then(() => console.log("OPML copied to clipboard!"))
        .catch(err => console.error("Failed to copy OPML:", err));
    }
  }
}

// Auto-execute in Node.js environment
if (typeof window === 'undefined') {
  main().catch(console.error);
}

// For browser console usage, just call main()
// main();